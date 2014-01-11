package com.infoblox.tapestry;

import java.text.FieldPosition;
import java.text.Format;
import java.text.ParsePosition;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.LinkedList;
import java.util.Locale;

import org.json.JSONArray;
import org.json.JSONException;

import com.androidplot.xy.BoundaryMode;
import com.androidplot.xy.LineAndPointFormatter;
import com.androidplot.xy.SimpleXYSeries;
import com.androidplot.xy.XYPlot;

import android.net.Uri;
import android.os.Bundle;
import android.util.FloatMath;
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
import android.graphics.Color;
import android.graphics.PointF;

public class NCIActivity extends Activity implements OnTouchListener{
    
    private EditText tapesty_url;
    private ArrayList<String> tapestryUrls;
    private ListView urlslist;
    private TextView helpView;
    private TapestryConnector tapestryConnector;
    private XYPlot plot;
    private SimpleXYSeries plotSeries;
    private RangesViewModel rangesViewModel;
    
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
        paintGraph();
        tapestryConnector = new TapestryConnector(this, plot, plotSeries);
        tapestryConnector.connectTapestry("ws://" + "epamove.herokuapp.com");
        
        rangesViewModel = new RangesViewModel(this);

    }
    
    public void paintGraph(){
        plot = (XYPlot) findViewById(R.id.simpleXYPlot);
        plot.getLegendWidget().setVisible(false);
        plot.getBackgroundPaint().setColor(Color.TRANSPARENT);
        plot.getGraphWidget().getBackgroundPaint().setColor(Color.TRANSPARENT);
        plot.getGraphWidget().getGridBackgroundPaint().setColor(Color.TRANSPARENT);
        plot.setBorderStyle(XYPlot.BorderStyle.NONE, null, null);
        float w = getWindowManager().getDefaultDisplay().getWidth();
        float r = w/200;
        plot.setDomainStepValue(r);
        
        plot.setDomainValueFormat(new Format() {

            private SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MMM-dd HH-mm-ss", Locale.getDefault());
            
            @Override
            public StringBuffer format(Object obj, StringBuffer toAppendTo, FieldPosition pos) {
                long timestamp = (((Number) obj).longValue() + 1389000000) * 1000;
                Date date = new Date(timestamp);
                return dateFormat.format(date, toAppendTo, pos);
            }

            @Override
            public Object parseObject(String source, ParsePosition pos) {
                return null;

            }
        });
        
        LineAndPointFormatter  series1Format = new LineAndPointFormatter(Color.rgb(0, 0, 255), 
                Color.rgb(0, 0, 255),  Color.argb(25, 0, 0, 255), null);        
        plotSeries = new SimpleXYSeries(new LinkedList<Long>(), new LinkedList<Long>(), null);                           
        plot.addSeries(plotSeries, series1Format);
        
        plot.setOnTouchListener(this);
        
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
    
    
    private PointF minXY;
    private PointF maxXY;
    
    // Definition of the touch states
    static final int NONE = 0;
    static final int ONE_FINGER_DRAG = 1;
    static final int TWO_FINGERS_DRAG = 2;
    int mode = NONE;
 
    PointF firstFinger;
    float lastScrolling;
    float distBetweenFingers;
    float lastZooming;

    @Override
    public boolean onTouch(View v, MotionEvent event) {
        if (null == minXY){
            plot.calculateMinMaxVals();
            rangesViewModel.initDimensions(plot);
            minXY = new PointF(plot.getCalculatedMinX().floatValue(), plot.getCalculatedMinY().floatValue());
            maxXY = new PointF(plot.getCalculatedMaxX().floatValue(), plot.getCalculatedMaxY().floatValue());
        }
        
        switch (event.getAction() & MotionEvent.ACTION_MASK) {
        case MotionEvent.ACTION_DOWN: // Start gesture
            firstFinger = new PointF(event.getX(), event.getY());
            mode = ONE_FINGER_DRAG;
            break;
        case MotionEvent.ACTION_POINTER_DOWN: // second finger
            distBetweenFingers = spacing(event);
            // the distance check is done to avoid false alarms
            if (distBetweenFingers > 5f) {
                mode = TWO_FINGERS_DRAG;
            }
            break;
        case MotionEvent.ACTION_MOVE:
            if (mode == ONE_FINGER_DRAG) {
                PointF oldFirstFinger=firstFinger;
                firstFinger=new PointF(event.getX(), event.getY());
                lastScrolling=oldFirstFinger.x-firstFinger.x;
                scroll(lastScrolling);
                lastZooming=(firstFinger.y-oldFirstFinger.y)/plot.getHeight();
                if (lastZooming<0)
                    lastZooming=1/(1-lastZooming);
                else
                    lastZooming+=1;
                redrawChart();
 
            } else if (mode == TWO_FINGERS_DRAG) {
                float oldDist =distBetweenFingers; 
                distBetweenFingers=spacing(event);
                if (distBetweenFingers > 0){
                    lastZooming = oldDist/distBetweenFingers;
                    redrawChart();
                }
            }
            break;
        }
        return true;
    
    }
    
    private void redrawChart(){
        zoom(lastZooming);
       // if (minXVal <= minXY.x &&  maxXVal >= maxXY.x ){
            plot.setDomainBoundaries(minXY.x, maxXY.x, BoundaryMode.FIXED);
            rangesViewModel.redrawRanges(minXY.x, maxXY.x);
            plot.redraw();
      //  }
    }
    
    private void zoom(float scale) {
        float domainSpan = maxXY.x    - minXY.x;
        float domainMidPoint = maxXY.x        - domainSpan / 2.0f;
        float offset = domainSpan * scale/ 2.0f;
        minXY.x=domainMidPoint - offset;
        maxXY.x=domainMidPoint + offset;
    }
 
    private void scroll(float pan) {
        float domainSpan = maxXY.x    - minXY.x;
        float step = domainSpan / plot.getWidth();
        float offset = pan * step;
        minXY.x+= offset;
        maxXY.x+= offset;
    }
 
    private float spacing(MotionEvent event) {
        if (event.getPointerCount() < 2){
            return 0;
        }
        float x = event.getX(0) - event.getX(1);
        float y = event.getY(0) - event.getY(1);
        return FloatMath.sqrt(x * x + y * y);
    }
  
}
