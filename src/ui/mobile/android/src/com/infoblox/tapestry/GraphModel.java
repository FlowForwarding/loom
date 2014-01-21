package com.infoblox.tapestry;

import java.text.FieldPosition;
import java.text.Format;
import java.text.ParsePosition;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.LinkedList;
import java.util.Locale;

import android.graphics.Color;
import android.graphics.DashPathEffect;
import android.graphics.PointF;
import android.os.Build;
import android.util.FloatMath;
import android.view.MotionEvent;
import android.view.View;
import android.view.View.OnTouchListener;
import android.view.View.OnLayoutChangeListener;
import android.view.Window;
import android.widget.TextView;

import com.androidplot.util.ValPixConverter;
import com.androidplot.xy.BoundaryMode;
import com.androidplot.xy.LineAndPointFormatter;
import com.androidplot.xy.SimpleXYSeries;
import com.androidplot.xy.XYPlot;

public class GraphModel implements OnTouchListener{
    
    private NCIActivity activity;
    private XYPlot plot;
    private SimpleXYSeries plotSeries;
    private LineAndPointFormatter topSeriesFormat;
    
    public RangesViewModel rangesViewModel;
    
    public XYPlot bottomPlot;
    public SimpleXYSeries bottomPlotSeries;
    private LineAndPointFormatter bottomSeriesFormat;
    private TextView selectedPointText;
    private View selectedPoint;
    private PointF selectedPointF;

    public GraphModel(NCIActivity activity, final RangesViewModel rangesViewModel){
        this.activity = activity;
        this.rangesViewModel = rangesViewModel;
        bottomPlot = (XYPlot) activity.findViewById(R.id.bottomXYPlot);
        bottomPlot.setRangeStepValue(1);
        makeUpChart(bottomPlot);
        bottomSeriesFormat = new LineAndPointFormatter(Color.rgb(0, 0, 255), 
                Color.TRANSPARENT,  Color.argb(25, 0, 0, 255), null);
        bottomSeriesFormat.getLinePaint().setStrokeWidth(1);
        bottomPlotSeries  = new SimpleXYSeries(new LinkedList<Long>(), new LinkedList<Long>(), null); 
        bottomPlot.addSeries(bottomPlotSeries, bottomSeriesFormat);
        
        bottomPlot.addOnLayoutChangeListener(new OnLayoutChangeListener() {
            @Override
            public void onLayoutChange(View v, int left, int top, int right,
                    int bottom, int oldLeft, int oldTop, int oldRight,
                    int oldBottom) {
                rangesViewModel.setGraphWidth();
            }  
        });
       
        plot = (XYPlot) activity.findViewById(R.id.simpleXYPlot);
        plot.setRangeStepValue(5);
        makeUpChart(plot);
        topSeriesFormat = new LineAndPointFormatter(Color.rgb(0, 0, 255), 
                Color.TRANSPARENT,  Color.argb(25, 0, 0, 255), null); 
        topSeriesFormat.getLinePaint().setStrokeWidth(1);
        plotSeries = new SimpleXYSeries(new LinkedList<Long>(), new LinkedList<Long>(), null);                           
        plot.addSeries(plotSeries, topSeriesFormat);
        plot.setOnTouchListener(this);
        
        selectedPointText = (TextView) activity.findViewById(R.id.selectedPointView);
        selectedPoint = activity.findViewById(R.id.selectedPoint);  
  
    }
    
    private void makeUpChart(final XYPlot plot){
        plot.getLegendWidget().setVisible(false);
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.HONEYCOMB) {
            plot.setLayerType(View.LAYER_TYPE_SOFTWARE, null);
        }
        plot.getBackgroundPaint().setColor(Color.TRANSPARENT);
        plot.getGraphWidget().getBackgroundPaint().setColor(Color.TRANSPARENT);
        plot.getGraphWidget().getGridBackgroundPaint().setColor(Color.TRANSPARENT);
        
        plot.getGraphWidget().getRangeGridLinePaint().setColor(Color.BLACK);
        plot.getGraphWidget().getRangeGridLinePaint().setPathEffect(new DashPathEffect(new float[]{1,2}, 1));
        plot.getGraphWidget().getDomainGridLinePaint().setColor(Color.BLACK);
        plot.getGraphWidget().getDomainGridLinePaint().setPathEffect(new DashPathEffect(new float[]{1,2}, 1));
        plot.setBorderStyle(XYPlot.BorderStyle.NONE, null, null);
        
        float w = activity.getWindowManager().getDefaultDisplay().getWidth();
        float r = w/150;
        plot.setDomainStepValue(r);
        plot.setDomainValueFormat(new Format() {

            private SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MMM-dd HH:mm:ss", Locale.getDefault());
            
            @Override
            public StringBuffer format(Object obj, StringBuffer toAppendTo, FieldPosition pos) {
                if (plot.getCalculatedMaxX().floatValue() - plot.getCalculatedMinX().floatValue() < 60*60*24){
                    dateFormat =  new SimpleDateFormat("yyyy-MMM-dd HH:mm", Locale.getDefault());
                } else if (plot.getCalculatedMaxX().floatValue() - plot.getCalculatedMinX().floatValue() < 60*60*24*30){
                    dateFormat =  new SimpleDateFormat("yyyy-MMM-dd", Locale.getDefault());
                } else {
                    dateFormat =  new SimpleDateFormat("yyyy-MMM", Locale.getDefault());
                }
                long timestamp = seriesArgumentConvert(((Number) obj).longValue());
                Date date = new Date(timestamp);
                return dateFormat.format(date, toAppendTo, pos);
            }

            @Override
            public Object parseObject(String source, ParsePosition pos) {
                return null;

            }
        });
        
        plot.getGraphWidget().getDomainOriginLabelPaint().setColor(Color.BLACK);
        plot.getGraphWidget().getRangeOriginLabelPaint().setColor(Color.BLACK);
        plot.getGraphWidget().getDomainLabelPaint().setColor(Color.BLACK);
        plot.getGraphWidget().getRangeLabelPaint().setColor(Color.BLACK);
    }
    
    public void showDataForPeriod(long period){
        long nowMilliseconds = System.currentTimeMillis();
        long startMilliseconds = nowMilliseconds - period;
        setNewRanges(startMilliseconds/1000 - 1389000000, getCurTimeRange());
        rangesViewModel.redrawRanges(minXY.x, maxXY.x);
        
    }
    
    public void addLast(float argument, float value){
        if (plotSeries.size() == 1 && plotSeries.getX(0).floatValue() > argument) {
            plotSeries.removeFirst();
            bottomPlotSeries.removeFirst();
        }
        plotSeries.addLast(argument, value);
        bottomPlotSeries.addLast(argument, value);
       
    }
    
    public void redraw(){
        if (plotSeries.size()>1){
            plot.calculateMinMaxVals();
            if (null ==  minXY){
                minXY = new PointF(plotSeries.getX(0).floatValue(), plot.getCalculatedMinY().floatValue());
                maxXY = new PointF(getCurTimeRange(), plot.getCalculatedMaxY().floatValue()); 
            }
            if (reseted){
                plot.setDomainBoundaries(plotSeries.getX(0).floatValue(), getCurTimeRange(), BoundaryMode.FIXED);  
                bottomPlot.setDomainBoundaries(bottomPlotSeries.getX(0), getCurTimeRange(), BoundaryMode.FIXED);
            }
            reseted = false;
            rangesViewModel.redrawRanges();
        }
        plot.redraw();
        bottomPlot.redraw();
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
    SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MMM-dd HH:mm:ss", Locale.getDefault());

    @Override
    public boolean onTouch(View v, MotionEvent event) {
        if (plotSeries.size() < 2){
            return false;
        }
        switch (event.getAction() & MotionEvent.ACTION_MASK) {
        case MotionEvent.ACTION_DOWN: // Start gesture
            firstFinger = new PointF(event.getX(), event.getY());
            mode = ONE_FINGER_DRAG;
            setMinMax(); 
            float valYInGrig = event.getY() - plot.getGraphWidget().getGridRect().top;
            float valXInGrig = event.getX() - plot.getGraphWidget().getGridRect().left;
            if (valYInGrig < 0 || valXInGrig < 0){
                break;
            }
            double yVal = ValPixConverter.pixToVal(valYInGrig, maxXY.y, minXY.y,
                    plot.getGraphWidget().getGridRect().height(), false);
            double xVal = ValPixConverter.pixToVal(valXInGrig, minXY.x, maxXY.x, 
                    plot.getGraphWidget().getGridRect().width(), false);
            for (int i = 1; i<plotSeries.size(); i++){
                double curV = plotSeries.getX(i).floatValue();
                if (xVal < curV){
                    xVal = curV;
                    yVal = plotSeries.getY(i).longValue();
                    selectedPointF = new PointF((float)xVal, (float)yVal);
                    selectedPointText.setText("NCI: " + (int)yVal + "  " +  dateFormat.format(new Date(seriesArgumentConvert(xVal))));
                    drawPoint();
                    break;
                }
            }
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
                PointF oldFirstFinger = firstFinger;
                firstFinger = new PointF(event.getX(), event.getY());
                lastScrolling = oldFirstFinger.x-firstFinger.x;
                scroll(lastScrolling);
                redrawChart();
 
            } else if (mode == TWO_FINGERS_DRAG) {
                float oldDist = distBetweenFingers; 
                distBetweenFingers = spacing(event);
                if (distBetweenFingers > 0){
                    lastZooming = oldDist/distBetweenFingers;
                    zoom(lastZooming);
                    redrawChart();
                }
            }
            activity.resetZoom();
            break;
        }
        return true;
    
    }
    
    private void drawPoint(){
        if (null == selectedPointF){
            return;
        }
        float pointPosY = ValPixConverter.valToPix(selectedPointF.y, minXY.y, maxXY.y, 
                plot.getGraphWidget().getGridRect().height(), true);
        float pointPosX = ValPixConverter.valToPix(selectedPointF.x, minXY.x, maxXY.x, 
                plot.getGraphWidget().getGridRect().width(), false);
        
        int[] locTop = new int[2];
        activity.findViewById(R.id.nci_layout).getLocationInWindow(locTop);
        int topOffset= locTop[1];
        int[] loc = new int[2];
        plot.getLocationInWindow(loc);
        selectedPoint.setX(pointPosX + loc[0] + plot.getGraphWidget().getGridRect().left - 3);
        selectedPoint.setY(pointPosY + loc[1] + plot.getGraphWidget().getGridRect().top - topOffset - 3);
        selectedPoint.setVisibility(View.VISIBLE);
        selectedPoint.invalidate();
    }

    private void setMinMax(){
        plot.calculateMinMaxVals();
        minXY = new PointF(plot.getCalculatedMinX().floatValue(), plot.getCalculatedMinY().floatValue());
        maxXY = new PointF(plot.getCalculatedMaxX().floatValue(), plot.getCalculatedMaxY().floatValue()); 
    }
    
    private long seriesArgumentConvert(double argument){
        return (long)(argument + 1389000000) * 1000;
    }
    
    public long getCurTimeRange(){
        return System.currentTimeMillis()/1000 - 1389000000;
    }
    
    private void redrawChart(){
        plot.setDomainBoundaries(minXY.x, maxXY.x, BoundaryMode.FIXED);
        rangesViewModel.redrawRanges(minXY.x, maxXY.x);
        setMinMax();
        drawPoint();
        plot.redraw();
    }
    
    private void zoom(float scale) {
        float domainSpan = maxXY.x - minXY.x;
        float domainMidPoint = maxXY.x - domainSpan / 2.0f;
        float offset = domainSpan * scale/ 2.0f;
        minXY.x = domainMidPoint - offset;
        maxXY.x = domainMidPoint + offset;
        correctBoundaryValues();
    }
 
    private void scroll(float pan) {
        float domainSpan = maxXY.x    - minXY.x;
        float step = domainSpan / plot.getWidth();
        float offset = pan * step;
        minXY.x+= offset;
        maxXY.x+= offset;
        correctBoundaryValues();
    }
    
    private void correctBoundaryValues(){
        if (minXY.x < bottomPlot.getCalculatedMinX().floatValue()){
            minXY.x = bottomPlot.getCalculatedMinX().floatValue();
        }
        if (maxXY.x > getCurTimeRange()){
            maxXY.x = bottomPlot.getCalculatedMaxX().floatValue();
        }
    }
 
    private float spacing(MotionEvent event) {
        if (event.getPointerCount() < 2){
            return 0;
        }
        float x = event.getX(0) - event.getX(1);
        float y = event.getY(0) - event.getY(1);
        return FloatMath.sqrt(x * x + y * y);
    }
    
    public void setNewRanges(float newMinX, float newMaxX){
        if (null == minXY){
            return;
        }
        minXY.x = newMinX;
        maxXY.x = newMaxX;
        plot.setDomainBoundaries(newMinX, newMaxX, BoundaryMode.FIXED);
        setMinMax();
        drawPoint();
        plot.redraw();
    }
    
    boolean reseted = true;
    public void clearData(){
        reseted = true;
        selectedPointF = null;
        selectedPoint.setVisibility(View.INVISIBLE);
        selectedPointText.setText("");
        plot.removeSeries(plotSeries);
        plot.clear();
        plotSeries = new SimpleXYSeries(new LinkedList<Long>(), new LinkedList<Long>(), null);   
        plot.addSeries(plotSeries, topSeriesFormat);
        plot.redraw();
        
        bottomPlot.removeSeries(bottomPlotSeries);
        bottomPlot.clear();
        bottomPlotSeries = new SimpleXYSeries(new LinkedList<Long>(), new LinkedList<Long>(), null);   
        bottomPlot.addSeries(bottomPlotSeries, bottomSeriesFormat);
        bottomPlot.redraw();
        rangesViewModel.resetRanges();
    }

}
