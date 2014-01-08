package com.infoblox.tapestry;

import java.net.URI;

import org.json.JSONException;
import org.json.JSONObject;

import android.app.Activity;
import android.util.Log;
import android.widget.TextView;

import com.codebutler.android_websockets.WebSocketClient;

public class TapestryConnector {

    protected static final String TAG = "WEBSOCKET";
    private Activity activity;
    WebSocketClient clientWss;

    public TapestryConnector(Activity activity){
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
                    } else {
                        String time = "updated "  + jsonObj.getString("Time").replace("T", " ").replace("Z", "");
                        if (jsonObj.has("NCI")){
                            setLabel(nciValue, jsonObj.getString("NCI"));
                            setLabel(nciValueTime, time);
                        } else if (jsonObj.has("QPS")){
                            setLabel(qpsValue, jsonObj.getString("QPS"));
                            setLabel(qpsValueTime, time);
                        } else if (jsonObj.has("NEP")){
                            setLabel(endpointsValue, jsonObj.getString("NEP"));
                            setLabel(endpointsValueTime, time);
                        };
                    };
                } catch (JSONException e) {
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
