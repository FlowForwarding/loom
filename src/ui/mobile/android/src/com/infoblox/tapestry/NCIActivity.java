package com.infoblox.tapestry;

import java.util.ArrayList;
import java.util.Arrays;
import org.json.JSONArray;
import org.json.JSONException;

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

public class NCIActivity extends Activity{
    
    private EditText tapesty_url;
    private ArrayList<String> tapestryUrls;
    private ListView urlslist;
    private TextView helpView;
    private TapestryConnector tapestryConnector;
    
    private RangesViewModel rangesViewModel;
    private GraphModel graphModel;
    
    static String PREFS_NAME = "tapestryPrefs";
    static String PREFS_URLS = "tapestryUrls";
    
    private ArrayList<ZoomButton> zoomButtons;

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
                    if (tapesty_url.getText().toString().trim().isEmpty()){
                        if (tapestryUrls.size() > 2){
                            tapesty_url.setText(tapestryUrls.get(2));  
                        } else {
                            tapesty_url.setText(tapestryUrls.get(0));   
                        }
                    }
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
            public void onItemClick(AdapterView<?> arg0, View arg1, int position,
                    long arg3) {
                  String newUrl = tapestryUrls.get(position);
                  tapesty_url.setText(newUrl);
                  if (position > 1){
                      tapestryUrls.remove(position);
                      tapestryUrls.add(2, newUrl);
                      urlslist.invalidateViews();
                  }
                  if (position != 1){
                      resetZoomBtns();
                      tapestryConnector.connectTapestry("ws://" + newUrl);
                      tapesty_url.clearFocus();
                      InputMethodManager imm = (InputMethodManager) tapesty_url.getContext().getSystemService(Context.INPUT_METHOD_SERVICE); 
                      imm.hideSoftInputFromWindow(tapesty_url.getWindowToken(), 0);
                  }   
            }
            
        });
        rangesViewModel = new RangesViewModel(this);
        graphModel = new GraphModel(this, rangesViewModel);
        rangesViewModel.setGraphModel(graphModel);
        zoomButtons = new ArrayList<ZoomButton>();
        int[] btnsIds = {R.id.d1, R.id.d5, R.id.m1, R.id.m3, R.id.m6, R.id.y1, R.id.y5, R.id.y10};
        for (int btnId : btnsIds){
            ZoomButton zoomBtn = (ZoomButton) findViewById(btnId);
            zoomButtons.add(zoomBtn);
        }
        
        tapestryConnector = new TapestryConnector(this, graphModel);
      
        String urlToConnect;
        if (tapestryUrls.size() < 3){
            urlToConnect =  tapestryUrls.get(0);
        } else {
            urlToConnect =  tapestryUrls.get(2);
        }
        tapestryConnector.connectTapestry("ws://" + urlToConnect);
        tapesty_url.setText(urlToConnect);
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
    
    public void changeZoom(View v){
        ZoomButton curBtn =  ((ZoomButton)v);
        if (curBtn.isDisabled()){
            return;
        }
       for(ZoomButton zoomBtn: zoomButtons){
           if (!zoomBtn.isDisabled()){
               zoomBtn.deselect();
           }
       }
       curBtn.select();
       graphModel.showDataForPeriod(curBtn.getPeriod());
    }
    
    private void resetZoomBtns(){
        for(ZoomButton zoomBtn: zoomButtons){
            zoomBtn.reset();
        } 
    }
    
    public void disableNotAvailableZoomBtns(long period){
        for (int i = 1; i <zoomButtons.size(); i++ ){
            if (zoomButtons.get(i-1).getPeriod() > period/1000){
                zoomButtons.get(i).setDisabled();
            }
        }
    }
    
    public void connectTapestry(View v) {
        String url = tapesty_url.getText().toString().trim();
        if (url.isEmpty()){
            return;
        }
        int existsPos = tapestryUrls.lastIndexOf(url);
        if (existsPos > 1){
            tapestryUrls.remove(existsPos);
        }
        tapestryUrls.add(2, url);
        urlslist.invalidateViews();
        saveTapestryUrls(this, tapestryUrls);
        tapesty_url.clearFocus();
        InputMethodManager imm = (InputMethodManager) v.getContext().getSystemService(Context.INPUT_METHOD_SERVICE); 
        imm.hideSoftInputFromWindow(v.getWindowToken(), 0);
        resetZoomBtns();
        tapestryConnector.connectTapestry("ws://" + url);
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
        helpView.setX(loc[0] + getResources().getDimension(R.dimen.popup_xoffset));
        helpView.setY(loc[1] + getResources().getDimension(R.dimen.popup_yoffset)); 
        helpView.setVisibility(View.VISIBLE);
    }
  
}
