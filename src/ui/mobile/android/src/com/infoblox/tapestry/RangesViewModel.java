package com.infoblox.tapestry;

import com.androidplot.xy.XYPlot;

import android.app.Activity;
import android.graphics.PointF;
import android.view.MotionEvent;
import android.view.View;
import android.view.View.OnTouchListener;

public class RangesViewModel implements OnTouchListener{
    
    private View rangesView;
    private View leftRange;
    private View rightRange;
    private View leftAmputation;
    private View rightAmputation;
    
    private float minXVal;
    private float maxXVal;
    private float graphWidth;
  
    private GraphModel graphModel;
    
    public RangesViewModel(Activity activity){
        leftRange = activity.findViewById(R.id.leftRange);
        rightRange = activity.findViewById(R.id.rightRange);
        leftAmputation = activity.findViewById(R.id.leftAmputation);
        rightAmputation = activity.findViewById(R.id.rightAmputation);
        rangesView = activity.findViewById(R.id.rangesView);
        rangesView.setOnTouchListener(this);
    }
    
    public void initDimensions(XYPlot plot){
        minXVal = plot.getCalculatedMinX().floatValue();
        maxXVal = plot.getCalculatedMaxX().floatValue();
        graphWidth = plot.getGraphWidget().getGridRect().width();
    }

    public void redrawRanges(float newMinX, float newMaxX){
        leftRange.setX ((newMinX - minXVal)*graphWidth/(maxXVal - minXVal));
        rightRange.setX (graphWidth - (maxXVal - newMaxX)*graphWidth/(maxXVal - minXVal));
    }
    
    private  PointF fingerPoint;
    private boolean moveLeft;
    private boolean moveRight;

    @Override
    public boolean onTouch(View v, MotionEvent event) {
        switch (event.getAction() & MotionEvent.ACTION_MASK) {
        case MotionEvent.ACTION_DOWN:
            fingerPoint = new PointF(event.getX(), event.getY());
            if (leftRange.getX() >= event.getX()){
                moveLeft = true;
                moveRight = false;
            } else if (rightRange.getX() <= event.getX()){
                moveLeft = false;
                moveRight = true; 
            } else {
                moveLeft = false;
                moveRight = false;  
            }
            break;
        case MotionEvent.ACTION_MOVE:
            PointF oldFirstFinger = fingerPoint;
            fingerPoint = new PointF(event.getX(), event.getY());
            float diff = fingerPoint.x  - oldFirstFinger.x;
            if (moveLeft){
                leftRange.setX(leftRange.getX() + diff);
            } else if (moveRight){
                rightRange.setX(rightRange.getX() + diff);
            } else {
                leftRange.setX(leftRange.getX() + diff);
                rightRange.setX(rightRange.getX() + diff);
            }
            
            float newMinVal = leftRange.getX()*(maxXVal - minXVal)/graphWidth + minXVal;
            float newMaxVal = (rightRange.getX() - graphWidth)/graphWidth*(maxXVal - minXVal) + maxXVal;
            graphModel.setNewRanges(newMinVal, newMaxVal);
            break;
        }
        return true;
    }
    
    public void setGraphModel(GraphModel graphModel) {
        this.graphModel = graphModel;
    }
}
