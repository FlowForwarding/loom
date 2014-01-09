package com.infoblox.tapestry;

import java.net.URI;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Date;

import org.json.JSONException;
import org.json.JSONObject;

import android.app.Activity;
import android.graphics.Color;
import android.util.Log;
import android.widget.TextView;

import com.androidplot.xy.LineAndPointFormatter;
import com.androidplot.xy.SimpleXYSeries;
import com.androidplot.xy.XYPlot;
import com.androidplot.xy.XYSeries;
import com.codebutler.android_websockets.WebSocketClient;

public class TapestryConnector {

    protected static final String TAG = "WEBSOCKET";
    private Activity activity;
    WebSocketClient clientWss;
    private String websocketMoreData1 = "{\"request\":\"more_data\",\"start\": \"";
    private String websocketMoreData2 = "Z\",\"end\": \"";
    private String websocketMoreData3 = "Z\",\"max_items\": \"5\"}";
    
    private XYPlot plot;

    public TapestryConnector(Activity activity){
        this.activity = activity;

        plot = (XYPlot) activity.findViewById(R.id.mySimpleXYPlot);
        plot.getLegendWidget().setVisible(false);
        plot.getBackgroundPaint().setColor(Color.TRANSPARENT);
        plot.getGraphWidget().getBackgroundPaint().setColor(Color.TRANSPARENT);
        plot.getGraphWidget().getGridBackgroundPaint().setColor(Color.TRANSPARENT);
        plot.setBorderStyle(XYPlot.BorderStyle.NONE, null, null);
        
        // Create a couple arrays of y-values to plot:
        Number[] series1Numbers = {1, 8, 5, 2, 7, 4};
   
 
        // Turn the above arrays into XYSeries':
        XYSeries series1 = new SimpleXYSeries(
                Arrays.asList(series1Numbers),          // SimpleXYSeries takes a List so turn our array into a List
                SimpleXYSeries.ArrayFormat.Y_VALS_ONLY, // Y_VALS_ONLY means use the element index as the x value
                null);                             // Set the display title of the series
 
        LineAndPointFormatter  series1Format = new LineAndPointFormatter(Color.rgb(0, 0, 255), 
                Color.rgb(0, 0, 255), 
                Color.argb(25, 0, 0, 255), null);
 
        // add a new series' to the xyplot:
        plot.addSeries(series1, series1Format);
 
        // reduce the number of range labels
        plot.setTicksPerRangeLabel(3);
 
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
                        SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
                        Date now = new Date(System.currentTimeMillis());
                        Date start = new Date(System.currentTimeMillis() - 1000*60*60*24);
                        clientWss.send(websocketMoreData1 + dateFormat.format(start).replace(" ", "T") +
                                websocketMoreData2 + dateFormat.format(now).replace(" ", "T") +
                                websocketMoreData3);
                            } else {
                                SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
                                String[] items = arg0.substring(1, arg0.length() -1) .split(",");
                                if (items.length > 2) {
                                    for (int i = 0; i < items.length/2; i++){
                                        String timeString = items[i*2].substring(8, items[i*2].length()-2).replace("T", " ");
                                        String valueString = items[i*2 + 1].substring(6, items[i*2 + 1].length());
                                        Date time = dateFormat.parse(timeString);
                                        timeString.charAt(0);
                                    }
//"Time":"2014-01-08T20:40:10Z"
//"NCI":12
                                } else {
                                    String time = "updated " + jsonObj.getString("Time").replace("T", " ").replace("Z", "");
                                    if (jsonObj.has("NCI")) {
                                        setLabel(nciValue, jsonObj.getString("NCI"));
                                        setLabel(nciValueTime, time);
                                    } else if (jsonObj.has("QPS")) {
                                        setLabel(qpsValue, jsonObj.getString("QPS"));
                                        setLabel(qpsValueTime, time);
                                    } else if (jsonObj.has("NEP")) {
                                        setLabel(endpointsValue, jsonObj.getString("NEP"));
                                        setLabel(endpointsValueTime, time);
                                    }
                                    ;
                                }
                                ;
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
