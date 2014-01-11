package com.infoblox.tapestry;

import java.net.URI;
import java.util.Date;
import java.util.Locale;
import java.util.TimeZone;

import org.json.JSONException;
import org.json.JSONObject;

import android.annotation.SuppressLint;
import android.app.Activity;
import android.graphics.Color;
import android.util.Log;
import android.widget.TextView;
import java.text.*;
import com.codebutler.android_websockets.WebSocketClient;

public class TapestryConnector {

    protected static final String TAG = "WEBSOCKET";
    private Activity activity;
    private WebSocketClient clientWss;
    private String websocketMoreData1 = "{\"request\":\"more_data\",\"start\": \"";
    private String websocketMoreData2 = "Z\",\"end\": \"";
    private String websocketMoreData3 = "Z\",\"max_items\": \"5\"}";
    
    private GraphModel graphModel;
    
    final TextView nciValue;
    final TextView qpsValue;
    final TextView endpointsValue;
    final TextView nciValueTime;
    final TextView qpsValueTime;
    final TextView endpointsValueTime;
    
    float curNCIValue = -1;
    float curNEPValue = -1;
    float curQPSValue = -1;

    public TapestryConnector(Activity activity, GraphModel graphModel){
        this.graphModel = graphModel;
        this.activity = activity;
        nciValue = (TextView) activity.findViewById(R.id.nciValue);
        qpsValue = (TextView) activity.findViewById(R.id.qpsValue);
        endpointsValue = (TextView) activity.findViewById(R.id.endpointsValue);
        nciValueTime = (TextView) activity.findViewById(R.id.nciUpdated);
        qpsValueTime = (TextView) activity.findViewById(R.id.qpsUpdated);
        endpointsValueTime  = (TextView) activity.findViewById(R.id.endpointsUpdated);
    }
    
    public void connectTapestry(String url) {
        graphModel.clearData();
        
        if (null != clientWss){
            clientWss.disconnect();
        }
        finishDemo();
        if (url.equals("ws://demo")){
            startDemo();
        }
        
        clientWss = new WebSocketClient(URI.create(url), new WebSocketClient.Listener() {

            @Override
            public void onConnect() {
                Log.d(TAG, "connect");
                clientWss.send("START_DATA");
           
            }

            @Override
            public void onDisconnect(int arg0, String arg1) {
                Log.d(TAG,"disconnect"); 

            }

            @Override
            public void onError(Exception arg0) {
                Log.d(TAG, arg0.getLocalizedMessage());
            }

            @Override
            public void onMessage(String arg0) {
                parseResponse(arg0);
            }

            @Override
            public void onMessage(byte[] arg0) {
                Log.e(TAG, "some byte message"); 
            }

        }, null);
        
        clientWss.connect();
    }
    
    public void parseResponse(String responseString){
        final SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss", Locale.getDefault());
        dateFormat.setTimeZone(TimeZone.getTimeZone("GTM"));
        try {
            JSONObject jsonObj = new JSONObject(responseString);
            if (jsonObj.has("start_time")){
                jsonObj.get("start_time");
                Date now = new Date(System.currentTimeMillis());
                Date start = new Date(System.currentTimeMillis() - 1000*60);
                clientWss.send(websocketMoreData1 + dateFormat.format(start).replace(" ", "T") +
                        websocketMoreData2 + dateFormat.format(now).replace(" ", "T") +
                        websocketMoreData3);
                    } else {
                        String[] items = responseString.substring(1, responseString.length() -1) .split(",");
                        if (items.length > 2) {
                            for (int i = 0; i < items.length/2; i++){
                                String timeString = items[i*2].substring(8, items[i*2].length()-2).replace("T", " ");
                                String valueString = items[i*2 + 1].substring(6, items[i*2 + 1].length());
                                Date time = dateFormat.parse(timeString);
                                graphModel.addLast(time.getTime()/1000 - 1389000000, Long.parseLong(valueString));
                            }
                            graphModel.redraw();
                        } else {
                            String time = "updated " + jsonObj.getString("Time").replace("T", " ").replace("Z", "");
                            if (jsonObj.has("NCI")) {
                                String valueString = jsonObj.getString("NCI");
                                float newValue = Float.parseFloat(valueString);
                                setValueLabel(nciValue, newValue, curNCIValue);
                                setLabel(nciValueTime, time);
                                String timeString = jsonObj.getString("Time").replace("T", " ").replace("Z", "");
                                Date dateTime = dateFormat.parse(timeString);
                                curNCIValue = newValue;
                              //  graphModel.addLast(dateTime.getTime()/1000 - 1389000000, curNCIValue);
                              //  graphModel.redraw();
                            } else if (jsonObj.has("QPS")) {
                                float newValue = Float.parseFloat(jsonObj.getString("QPS"));
                                setValueLabel(qpsValue, newValue, curQPSValue);
                                setLabel(qpsValueTime, time);
                                curQPSValue = newValue;
                            } else if (jsonObj.has("NEP")) {
                                float newValue = Float.parseFloat(jsonObj.getString("NEP"));
                                setValueLabel(endpointsValue, newValue, curNEPValue);
                                setLabel(endpointsValueTime, time);
                                curNEPValue = newValue;
                            };
                        };
            };
        } catch (JSONException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (ParseException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

    }
    
    public void setValueLabel(final TextView label,final float newValue, final float curValue){
        activity.runOnUiThread(new Runnable(){
            @SuppressLint("ResourceAsColor")
            public void run() {
                if (curValue == -1 || curValue == newValue){
                    label.setTextColor(Color.BLACK);
                } else if (curValue < newValue){
                    label.setTextColor(Color.RED);
                } else if (curValue > newValue){
                    label.setTextColor(activity.getResources().getColor(R.color.tapestrygreen));
                }
                label.setText(String.valueOf((int)newValue));
            }         
        });
    }
    
    public void setLabel(final TextView label,final String text){
        activity.runOnUiThread(new Runnable(){
            public void run() {
                label.setText(text);
            }         
        });
    }
    
    private boolean isDemoMode = false;
    
    private void finishDemo(){
        isDemoMode = false;
    }
    
    private void startDemo(){
        isDemoMode = true;
        final SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss", Locale.getDefault());
        dateFormat.setTimeZone(TimeZone.getTimeZone("GTM"));
        StringBuffer responseText = new StringBuffer("{");
        int numberOfPoints = 10;
        int timePeriod = 1000*60*60*12;
        int dateStep = timePeriod/numberOfPoints;
        for (int i = 0; i < numberOfPoints; i++){
            Date curDate = new Date(System.currentTimeMillis() - timePeriod + dateStep*i);
            String dateString = dateFormat.format(curDate).replace(" ", "T");
            responseText.append( "\"Time\":\"" + dateString + "Z" + "\",\"NCI\":" +  (int)(Math.random()*5) + ",");
        }
        responseText.replace(responseText.length() - 1, responseText.length(), "}");
        parseResponse(responseText.toString());
        
        Runnable r = new Runnable() {
            public void run() {
                while (isDemoMode){
                    try {
                        Date curDate = new Date(System.currentTimeMillis());
                        String dateString = dateFormat.format(curDate).replace(" ", "T");
                        String responseString = "{\"NCI\":"+ (int)(Math.random()*5) + ",\"Time\":\"" + dateString + "Z\"}";
                        parseResponse(responseString);
                        responseString = "{\"NEP\":"+ (int)(Math.random()*5) + ",\"Time\":\"" + dateString + "Z\"}";
                        parseResponse(responseString);
                        responseString = "{\"QPS\":"+ (int)(Math.random()*5) + ",\"Time\":\"" + dateString + "Z\"}";
                        parseResponse(responseString);
                        Thread.sleep(5*1000);
                    } catch (InterruptedException e) {
                        e.printStackTrace();
                    }
                }
            }
        };

        new Thread(r).start();
    }
}
