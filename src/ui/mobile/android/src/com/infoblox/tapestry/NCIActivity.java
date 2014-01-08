package com.infoblox.tapestry;

import java.util.ArrayList;
import java.util.Arrays;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import android.net.Uri;
import android.os.Bundle;
import android.view.MotionEvent;
import android.view.View;
import android.view.View.OnFocusChangeListener;
import android.view.View.OnTouchListener;
import android.view.inputmethod.InputMethodManager;
import android.widget.AdapterView;
import android.widget.EditText;
import android.widget.ImageView;
import android.widget.ListView;
import android.widget.RelativeLayout;
import android.widget.TextView;
import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.content.res.Resources;

public class NCIActivity extends Activity {
    
    EditText tapesty_url;
    ArrayList<String> tapestryUrls;
    ListView urlslist;
    TextView helpView;
    static String PREFS_NAME = "tapestryPrefs";
    static String PREFS_URLS = "tapestryUrls";

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_nci);
        
        helpView = (TextView) findViewById(R.id.hintView);
        
        final ListView infoList = (ListView) findViewById(R.id.infolist);
        final Resources res = getResources();
        
        String[] labels = {res.getString(R.string.about_nci),
                res.getString(R.string.nci_technical_paper),
                res.getString(R.string.about_flowforwarding)};
        
        final String[] helpUrls = {res.getString(R.string.about_nci_url),
                res.getString(R.string.nci_technical_paper_url),
                res.getString(R.string.about_flowforwarding_url)};
        
        InfoListAdapter adapter = new InfoListAdapter(this, labels);
        infoList.setAdapter(adapter);
        infoList.setOnItemClickListener(new AdapterView.OnItemClickListener() {

            @Override
            public void onItemClick(AdapterView<?> arg0, View view, int position,
                    long id) {
               String url = helpUrls[position];
               Intent browserIntent = new Intent(Intent.ACTION_VIEW, Uri.parse(url));
               startActivity(browserIntent);
            } 
            
        });
        
        urlslist = (ListView) findViewById(R.id.urlslist);
        tapesty_url = (EditText) findViewById(R.id.tapesty_url);
        final ImageView goaction = (ImageView) findViewById(R.id.goaction);
        final ImageView clearaction = (ImageView) findViewById(R.id.clearaction);
        tapesty_url.setOnFocusChangeListener(new OnFocusChangeListener(){
            @Override
            public void onFocusChange(View v, boolean hasFocus) {
                if (hasFocus){
                    urlslist.setVisibility(View.VISIBLE);
                    clearaction.setVisibility(View.VISIBLE);
                    infoList.setVisibility(View.INVISIBLE);
                    goaction.setVisibility(View.VISIBLE);
                    helpView.setVisibility(View.GONE);
                } else {
                    urlslist.setVisibility(View.GONE);
                    clearaction.setVisibility(View.INVISIBLE);
                    goaction.setVisibility(View.INVISIBLE);
                }      
            }   
        });
        
        RelativeLayout nci_layout = (RelativeLayout)findViewById(R.id.nci_layout);
        nci_layout.setOnTouchListener( new OnTouchListener(){

            @Override
            public boolean onTouch(View v, MotionEvent event) {
                if (event.getAction() == MotionEvent.ACTION_DOWN) {
                    infoList.setVisibility(View.INVISIBLE);
                    tapesty_url.clearFocus();
                    helpView.setVisibility(View.GONE);
                    InputMethodManager imm = (InputMethodManager) v.getContext().getSystemService(Context.INPUT_METHOD_SERVICE); 
                    imm.hideSoftInputFromWindow(v.getWindowToken(), 0);
                }
                return false;
            }
            
        });
        String[] urls = {res.getString(R.string.demo),
                res.getString(R.string.nciexamplecom28080clientsockyaws)};
        SharedPreferences urlsPrefs = getSharedPreferences(PREFS_NAME, 0);
        String  tapestryUrlsString =  urlsPrefs.getString(PREFS_URLS, null);
        if ( null == tapestryUrlsString ){
            tapestryUrls = new ArrayList<String>(Arrays.asList(urls));
        } else { 
            try {
                tapestryUrls = new ArrayList<String>();
                JSONArray jsonUrls = new JSONArray(tapestryUrlsString);
                for (int i=0; i<jsonUrls.length(); i++) {
                    tapestryUrls.add( jsonUrls.getString(i) );
                }
            } catch (JSONException e) {
                e.printStackTrace();
            }
        }

        TapestryUrlsAdapter urlsAdapter = new TapestryUrlsAdapter(this, tapestryUrls);
        urlslist.setAdapter(urlsAdapter);
        urlslist.setOnItemClickListener(new AdapterView.OnItemClickListener() {

            @Override
            public void onItemClick(AdapterView<?> arg0, View arg1, int arg2,
                    long arg3) {
                // TODO Auto-generated method stub
                
            }
            
        });
     
    }

    public void toggleInfoMenu(View v) {
        View list = findViewById(R.id.infolist);
        if (list.getVisibility() == View.VISIBLE){
            list.setVisibility(View.INVISIBLE);
        } else {
            list.setVisibility(View.VISIBLE);
        }
    }
    
    public void clearTapestryUrl(View v){
        tapesty_url.setText("");
    }
    
    public void connectTapestry(View v) {
        String url = tapesty_url.getText().toString();
        int existsPos = tapestryUrls.lastIndexOf(url);
        if (existsPos > 1){
            tapestryUrls.remove(existsPos);
        }
        tapestryUrls.add(2, url);
        urlslist.invalidateViews();
        saveTapestryUrls(this, tapestryUrls);
    }
    
    public static void saveTapestryUrls(Context context, ArrayList<String> urls){
        SharedPreferences settings = context.getSharedPreferences(PREFS_NAME, 0);
        SharedPreferences.Editor editor = settings.edit();
        editor.putString(PREFS_URLS, new JSONArray(urls).toString());
        editor.commit();
    }
    
    public void showEndpointsHelp(View v) {
        setupHelpView(v);
        helpView.setText(R.string.numberOfConnectedNetworkElements);
    }
    
    public void showNciHelp(View v) {
        setupHelpView(v);
        helpView.setText(R.string.networkComplexityIndex);
    }
    
    public void showQpsHelp(View v) {
        setupHelpView(v);
        helpView.setText(R.string.successfulDNSQueryResponsesPerSecond);
    }
    
    private void setupHelpView(View v){
        int[] loc = new int[2];
        v.getLocationOnScreen(loc);
        helpView.setX(loc[0] + 30);
        helpView.setY(loc[1] + 70); 
        helpView.setVisibility(View.VISIBLE);
    }
}
