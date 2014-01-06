package com.infoblox.tapestry;

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
import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.content.res.Resources;

public class NCIActivity extends Activity {

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_nci);
        final ListView infoList = (ListView) findViewById(R.id.infolist);
        final Resources res = getResources();
        
        String[] labels = {res.getString(R.string.about_nci),
                res.getString(R.string.nci_technical_paper),
                res.getString(R.string.about_flowforwarding)};
        
        final String[] urls = {res.getString(R.string.about_nci_url),
                res.getString(R.string.nci_technical_paper_url),
                res.getString(R.string.about_flowforwarding_url)};
        
        InfoListAdapter adapter = new InfoListAdapter(this, labels);
        infoList.setAdapter(adapter);
        infoList.setOnItemClickListener(new AdapterView.OnItemClickListener() {

            @Override
            public void onItemClick(AdapterView<?> arg0, View view, int position,
                    long id) {
               String url = urls[position];
               Intent browserIntent = new Intent(Intent.ACTION_VIEW, Uri.parse(url));
               startActivity(browserIntent);
            } 
            
        });
        
        final EditText tapesty_url = (EditText) findViewById(R.id.tapesty_url);
        final ImageView goaction = (ImageView) findViewById(R.id.goaction);
        final ImageView clearaction = (ImageView) findViewById(R.id.clearaction);
        tapesty_url.setOnFocusChangeListener(new OnFocusChangeListener(){
            @Override
            public void onFocusChange(View v, boolean hasFocus) {
                if (hasFocus){
                    clearaction.setVisibility(View.VISIBLE);
                    infoList.setVisibility(View.INVISIBLE);
                    goaction.setVisibility(View.VISIBLE);
                } else {
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
                    InputMethodManager imm = (InputMethodManager) v.getContext().getSystemService(Context.INPUT_METHOD_SERVICE); 
                    imm.hideSoftInputFromWindow(v.getWindowToken(), 0);
                }
                return false;
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
        EditText tapesty_url = (EditText) findViewById(R.id.tapesty_url); 
        tapesty_url.setText("");
    }
    
    public void connectTapestry(View v) {
    
    }
}
