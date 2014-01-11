package com.infoblox.tapestry;

import java.text.FieldPosition;
import java.text.Format;
import java.text.ParsePosition;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.LinkedList;
import java.util.Locale;

import android.app.Activity;
import android.graphics.Color;
import android.graphics.PointF;
import android.util.FloatMath;
import android.view.MotionEvent;
import android.view.View;
import android.view.View.OnTouchListener;

import com.androidplot.xy.BoundaryMode;
import com.androidplot.xy.LineAndPointFormatter;
import com.androidplot.xy.SimpleXYSeries;
import com.androidplot.xy.XYPlot;

public class GraphModel implements OnTouchListener{
    
    public XYPlot plot;
    public SimpleXYSeries plotSeries;
    private RangesViewModel rangesViewModel;
    
    public GraphModel(Activity activity, RangesViewModel rangesViewModel){
        this.rangesViewModel = rangesViewModel;
        
        plot = (XYPlot) activity.findViewById(R.id.simpleXYPlot);
        plot.getLegendWidget().setVisible(false);
        plot.getBackgroundPaint().setColor(Color.TRANSPARENT);
        plot.getGraphWidget().getBackgroundPaint().setColor(Color.TRANSPARENT);
        plot.getGraphWidget().getGridBackgroundPaint().setColor(Color.TRANSPARENT);
        plot.setBorderStyle(XYPlot.BorderStyle.NONE, null, null);
        float w = activity.getWindowManager().getDefaultDisplay().getWidth();
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
