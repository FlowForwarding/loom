package com.infoblox.tapestry;

import java.net.URI;
import java.util.Date;
import java.util.Locale;
import java.util.TimeZone;

import org.json.JSONException;
import org.json.JSONObject;

import android.annotation.SuppressLint;
import android.graphics.Color;
import android.util.Log;
import android.view.View;
import android.widget.TextView;
import java.text.*;
import com.codebutler.android_websockets.WebSocketClient;

public class TapestryConnector {

    protected static final String TAG = "WEBSOCKET";
    private NCIActivity activity;
    private WebSocketClient clientWss;
    private String websocketMoreData1 = "{\"request\":\"more_data\",\"start\": \"";
    private String websocketMoreData2 = "Z\",\"end\": \"";
    private String websocketMoreData3 = "Z\",\"max_items\": \"5\"}";
    
    private GraphModel graphModel;
    
    private Date startDate;
    
    final TextView nciValue;
    final TextView qpsValue;
    final TextView endpointsValue;
    final TextView nciValueTime;
    final TextView qpsValueTime;
    final TextView endpointsValueTime;
    final View loadingLabel;
    final View tapToConnect;
    
    float curNCIValue = -1;
    float curNEPValue = -1;
    float curQPSValue = -1;

    public TapestryConnector(NCIActivity activity, GraphModel graphModel){
        this.graphModel = graphModel;
        this.activity = activity;
        nciValue = (TextView) activity.findViewById(R.id.nciValue);
        qpsValue = (TextView) activity.findViewById(R.id.qpsValue);
        endpointsValue = (TextView) activity.findViewById(R.id.endpointsValue);
        nciValueTime = (TextView) activity.findViewById(R.id.nciUpdated);
        qpsValueTime = (TextView) activity.findViewById(R.id.qpsUpdated);
        endpointsValueTime  = (TextView) activity.findViewById(R.id.endpointsUpdated);
        loadingLabel = activity.findViewById(R.id.loadingLabel);
        tapToConnect = activity.findViewById(R.id.tapToConnect);
    }
    
    boolean isDemo;
    
    public void connectTapestry(String url) {
        isDemo = url.equals("ws://demo");
        if (null != clientWss){
            clientWss.disconnect();
        }
        finishDemo();
        clearData();
        graphModel.clearData();
        loadingLabel.setVisibility(View.VISIBLE);
        tapToConnect.setVisibility(View.INVISIBLE);
        if (url.equals("ws://demo")){
            startDemo();
            return;
        }
        
        clientWss = new WebSocketClient(URI.create(url), new WebSocketClient.Listener() {

            @Override
            public void onConnect() {
                Log.d(TAG, "connect");
                clientWss.send("START_DATA");
            }

            @Override
            public void onDisconnect(int arg0, String arg1) {
                activity.runOnUiThread(new Runnable(){
                    @Override
                    public void run() {
                        tapToConnect.setVisibility(View.VISIBLE);
                    }
                });
            }

            @Override
            public void onError(Exception arg0) {
                if (!isDemo){
                    activity.runOnUiThread(new Runnable(){
                        @Override
                        public void run() {
                            tapToConnect.setVisibility(View.VISIBLE);
                        }
                    });
                }
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
                String startDateString = jsonObj.getString("start_time").replace("T", " ").replace("Z", "");
                Date now = new Date(System.currentTimeMillis());
                startDate = dateFormat.parse(startDateString);
                activity.disableNotAvailableZoomBtns(System.currentTimeMillis() - startDate.getTime());
                if (!isDemo){
                    clientWss.send(websocketMoreData1 + dateFormat.format(startDate).replace(" ", "T") +
                        websocketMoreData2 + dateFormat.format(now).replace(" ", "T") +
                        websocketMoreData3);
                }
                    } else {
                        String[] items = responseString.substring(1, responseString.length() -1) .split(",");
                        if (items.length > 2) {
                            activity.runOnUiThread(new Runnable(){
                                @Override
                                public void run() {
                                    loadingLabel.setVisibility(View.INVISIBLE);
                                }
                            });
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
                                graphModel.addLast(dateTime.getTime()/1000 - 1389000000, curNCIValue);
                                graphModel.redraw();
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
                String valueForUi;
                if (newValue > 1000000000){
                    valueForUi = String.valueOf(((int)newValue/10000000)/10.0) + " B";
                } else if (newValue > 1000000){
                    valueForUi = String.valueOf(((int)newValue/100000)/10.0) + " M";
                } else if (newValue > 1000){
                    valueForUi = String.valueOf(((int)newValue/100)/10.0) + " K";
                } else {
                    valueForUi = String.valueOf((int)newValue);
                }
                label.setText(valueForUi);
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
        int trendMiddle = 6;
        int trendStepCounter = 0;
        int numberOfPoints = 100;
        int timePeriod = 1000*60*5;
        parseResponse("{\"start_time\":\"" + 
                dateFormat.format(new Date(System.currentTimeMillis() - timePeriod)).replace(" ", "T") + "Z\"}");
        
        int dateStep = timePeriod/numberOfPoints;
        for (int i = 0; i <= numberOfPoints; i++){
            if (trendStepCounter > 5){
                trendStepCounter = 0;
                trendMiddle++;
            }
            trendStepCounter++;
            Date curDate = new Date(System.currentTimeMillis() - timePeriod + dateStep*i);
            String dateString = dateFormat.format(curDate).replace(" ", "T");
            responseText.append( "\"Time\":\"" + dateString + "Z" + "\",\"NCI\":" +  (trendMiddle + (int)(Math.random()*5)) + ",");
        }
        responseText.replace(responseText.length() - 1, responseText.length(), "}");
        parseResponse(responseText.toString());
        
        final int trendVal = trendMiddle;
        Runnable r = new Runnable() {
            public void run() {
                while (isDemoMode){
                    try {
                        Date curDate = new Date(System.currentTimeMillis());
                        String dateString = dateFormat.format(curDate).replace(" ", "T");
                        String responseString = "{\"NCI\":" + (int)(trendVal + Math.random()*5) + ",\"Time\":\"" + dateString + "Z\"}";
                        parseResponse(responseString);
                        responseString = "{\"NEP\":"+ (int)(2000 + (Math.random()*400)) + ",\"Time\":\"" + dateString + "Z\"}";
                        parseResponse(responseString);
                        responseString = "{\"QPS\":"+ (int)(600000000 + (Math.random()*50000000)) + ",\"Time\":\"" + dateString + "Z\"}";
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
    
    public void clearData(){
        curNCIValue = -1;
        curNEPValue = -1;
        curQPSValue = -1;  
        nciValue.setText("");
        qpsValue.setText("");
        endpointsValue.setText("");
        nciValueTime.setText("");
        qpsValueTime.setText("");
        endpointsValueTime.setText("");
    }
}
