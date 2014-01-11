package com.infoblox.tapestry;

import java.net.URI;
import java.util.Date;
import java.util.Locale;
import java.util.TimeZone;

import org.json.JSONException;
import org.json.JSONObject;

import android.app.Activity;
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

    public TapestryConnector(Activity activity, GraphModel graphModel){
        this.graphModel = graphModel;
        this.activity = activity;
    }
    
    public void connectTapestry(String url) {
        final TextView nciValue = (TextView) activity.findViewById(R.id.nciValue);
        final TextView qpsValue = (TextView) activity.findViewById(R.id.qpsValue);
        final TextView endpointsValue = (TextView) activity.findViewById(R.id.endpointsValue);
        final TextView nciValueTime = (TextView) activity.findViewById(R.id.nciUpdated);
        final TextView qpsValueTime = (TextView) activity.findViewById(R.id.qpsUpdated);
        final TextView endpointsValueTime  = (TextView) activity.findViewById(R.id.endpointsUpdated);
        
        if (null != clientWss){
            clientWss.disconnect();
        }
        final SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss", Locale.getDefault());
        dateFormat.setTimeZone(TimeZone.getTimeZone("GTM"));
        
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
                try {
                    JSONObject jsonObj = new JSONObject(arg0);
                    if (jsonObj.has("start_time")){
                        jsonObj.get("start_time");
                        Date now = new Date(System.currentTimeMillis());
                        Date start = new Date(System.currentTimeMillis() - 1000*60);
                        clientWss.send(websocketMoreData1 + dateFormat.format(start).replace(" ", "T") +
                                websocketMoreData2 + dateFormat.format(now).replace(" ", "T") +
                                websocketMoreData3);
                            } else {
                                String[] items = arg0.substring(1, arg0.length() -1) .split(",");
                                if (items.length > 2) {
                                    for (int i = 0; i < items.length/2; i++){
                                        String timeString = items[i*2].substring(8, items[i*2].length()-2).replace("T", " ");
                                        String valueString = items[i*2 + 1].substring(6, items[i*2 + 1].length());
                                        Date time = dateFormat.parse(timeString);
                                        graphModel.plotSeries.addLast(time.getTime()/1000 - 1389000000, Long.parseLong(valueString));
                                    }
                                    graphModel.plot.redraw();
                                } else {
                                    String time = "updated " + jsonObj.getString("Time").replace("T", " ").replace("Z", "");
                                    if (jsonObj.has("NCI")) {
                                        String valueString = jsonObj.getString("NCI");
                                        setLabel(nciValue, valueString);
                                        setLabel(nciValueTime, time);
                                        String timeString = jsonObj.getString("Time").replace("T", " ").replace("Z", "");
                                        Date dateTime = dateFormat.parse(timeString);
                                        if (graphModel.plotSeries.size()>0){
                                            graphModel.plotSeries.addLast(dateTime.getTime()/1000 - 1389000000, Long.parseLong(valueString));
                                            graphModel.plot.redraw();
                                        }
                                    } else if (jsonObj.has("QPS")) {
                                        setLabel(qpsValue, jsonObj.getString("QPS"));
                                        setLabel(qpsValueTime, time);
                                    } else if (jsonObj.has("NEP")) {
                                        setLabel(endpointsValue, jsonObj.getString("NEP"));
                                        setLabel(endpointsValueTime, time);
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

            @Override
            public void onMessage(byte[] arg0) {
                Log.e(TAG, "some byte message"); 
            }

        }, null);
        
        clientWss.connect();
    }
    
    public void setLabel(final TextView label,final String text){
        activity.runOnUiThread(new Runnable(){
            public void run() {
                label.setText(text);
            }         
        });
    }

}
