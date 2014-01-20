package com.infoblox.tapestry;

import android.graphics.PointF;
import android.view.MotionEvent;
import android.view.View;
import android.view.View.OnTouchListener;
import android.widget.RelativeLayout;

public class RangesViewModel implements OnTouchListener{
    
    private View rangesView;
    private View leftRange;
    private View rightRange;
    private View leftAmputation;
    private View rightAmputation;
    
    private float minXVal;
    private float maxXVal;
    private float graphWidth = -1;
    private float leftIndent;
    private int rangeHeight;
    private int rangeY;
    
    private int minRangesDistance = 3;
  
    private GraphModel graphModel;
    private NCIActivity  activity;
    
    public RangesViewModel(NCIActivity activity){
        this.activity = activity;
        leftRange = activity.findViewById(R.id.leftRange);
        rightRange = activity.findViewById(R.id.rightRange);
        leftAmputation = activity.findViewById(R.id.leftAmputation);
        rightAmputation = activity.findViewById(R.id.rightAmputation);
        rangesView = activity.findViewById(R.id.rangesView);
        rangesView.setOnTouchListener(this);
    }
    
    public void setGraphWidth(){
        if  (graphWidth == -1 && null != graphModel.bottomPlot.getGraphWidget().getGridRect()){
            graphWidth = graphModel.bottomPlot.getGraphWidget().getGridRect().width();
            leftIndent = graphModel.bottomPlot.getGraphWidget().getGridRect().left;
            rangeHeight = (int) graphModel.bottomPlot.getGraphWidget().getGridRect().height();
            rangeY = (int) graphModel.bottomPlot.getGraphWidget().getGridRect().top;
            activity.runOnUiThread(new Runnable(){
                public void run() {
                    leftAmputation.setX(leftIndent);
                    leftAmputation.setY(rangeY);
                    rightAmputation.setY(rangeY);
                    leftRange.setY(rangeY);
                    leftRange.setX(leftIndent);
                    leftRange.getLayoutParams().height = rangeHeight;
                    leftRange.invalidate();
                    
                    rightRange.getLayoutParams().height = rangeHeight;
                    rightRange.setY(rangeY);
                    rightRange.setX(leftIndent + graphWidth);
                    rightRange.invalidate();  
                    
                    redrawRanges();
                }
            }); 
        }
    }
    
    float leftRangeVal;
    float rightRangeVal;
    
    public void redrawAmputations(){
        RelativeLayout.LayoutParams params = new RelativeLayout.LayoutParams((int) (leftRange.getX() - leftIndent), rangeHeight);
        leftAmputation.setLayoutParams(params);
        RelativeLayout.LayoutParams params2 = new RelativeLayout.LayoutParams((int) (graphWidth + leftIndent - rightRange.getX()), rangeHeight);
        rightAmputation.setX(rightRange.getX());
        rightAmputation.setLayoutParams(params2);
    }
    
    public void resetRanges(){
        if (null != graphModel.bottomPlot.getGraphWidget().getGridRect()){

            leftRangeVal = -1;
            rightRangeVal = -1;
            redrawAmputations();
        }
    }
    
    public void setDefaultRanges(){
        graphModel.bottomPlot.calculateMinMaxVals();
        minXVal = graphModel.bottomPlot.getCalculatedMinX().floatValue();
        maxXVal = getCurTimeRange();
        leftRangeVal = maxXVal - (maxXVal - minXVal)/10;
        rightRangeVal = maxXVal;
        redrawRanges(leftRangeVal, rightRangeVal);
    }
    
    public void redrawRanges(){
        if (leftRangeVal == -1){
            graphModel.bottomPlot.calculateMinMaxVals();
            minXVal = graphModel.bottomPlot.getCalculatedMinX().floatValue();
            maxXVal = getCurTimeRange();
            leftRangeVal = minXVal;
            rightRangeVal = maxXVal;
        }
        if (rightRange.getX() < graphWidth + leftIndent){
            redrawRanges(leftRangeVal, rightRangeVal);
        } else {
            setDefaultRanges();
        }
    }
    
    public long getCurTimeRange(){
        return System.currentTimeMillis()/1000 - 1389000000;
    }

    public void redrawRanges(final float newMinTopX, final float newMaxTopX){
        graphModel.bottomPlot.calculateMinMaxVals();
        minXVal = graphModel.bottomPlot.getCalculatedMinX().floatValue();
        maxXVal = getCurTimeRange();
        leftRangeVal = newMinTopX < minXVal ? minXVal : newMinTopX;
        rightRangeVal = newMaxTopX;
        
        if (-1 != graphWidth){
            activity.runOnUiThread(new Runnable(){
                public void run() {
                    leftRange.setX(leftIndent +  (newMinTopX - minXVal)*graphWidth/(maxXVal - minXVal));
                    rightRange.setX(leftIndent + (newMaxTopX - minXVal)*graphWidth/(maxXVal - minXVal));
                    graphModel.setNewRanges(leftRangeVal, rightRangeVal);
                    redrawAmputations();
                }
            }); 
        }
    }
    
    public float convertRange(float val){
        return (val - leftIndent)*(maxXVal - minXVal)/graphWidth + minXVal;
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
            float newLeftX = leftRange.getX();
            float newRightX = rightRange.getX();
            if (moveLeft){
                newLeftX = leftRange.getX() + diff;
            } else if (moveRight){
                newRightX = rightRange.getX() + diff;
            } else {
                newLeftX = leftRange.getX() + diff;
                newRightX = rightRange.getX() + diff;
            }
            if (newLeftX < leftIndent){
                newLeftX = leftIndent;
            }
            if (newRightX > leftIndent + graphWidth){
                newRightX = leftIndent + graphWidth;
            }
            if (newRightX - newLeftX < minRangesDistance){
                break;
            }
            
            leftRange.setX(newLeftX);
            rightRange.setX(newRightX);
            
            graphModel.bottomPlot.calculateMinMaxVals();
            minXVal = graphModel.bottomPlot.getCalculatedMinX().floatValue();
            maxXVal = getCurTimeRange();
            
            leftRangeVal = convertRange(leftRange.getX());
            rightRangeVal = convertRange(rightRange.getX());
            graphModel.setNewRanges(leftRangeVal, rightRangeVal);
            redrawAmputations();
            activity.resetZoom();
            break;
        }
        return true;
    }
    
    public void setGraphModel(GraphModel graphModel) {
        this.graphModel = graphModel;
    }
}
