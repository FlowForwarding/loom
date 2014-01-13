package com.infoblox.tapestry;

import android.app.Activity;
import android.graphics.PointF;
import android.util.Log;
import android.view.MotionEvent;
import android.view.View;
import android.view.View.OnTouchListener;
import android.widget.LinearLayout;
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
    private Activity activity;
    
    public RangesViewModel(Activity activity){
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
                    resetRanges();   
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
            leftAmputation.setX(leftIndent);
            leftAmputation.setY(rangeY);
            rightAmputation.setY(rangeY);
            leftRange.setY(rangeY);
            leftRange.setX(leftIndent);
            leftRange.getLayoutParams().height = rangeHeight;
            
            rightRange.getLayoutParams().height = rangeHeight;
            rightRange.setY(rangeY);
            rightRange.setX(leftIndent + graphWidth);

            leftRangeVal = convertRange(leftRange.getX());
            rightRangeVal = convertRange(rightRange.getX());
            redrawAmputations();
        }
    }
    
    public void redrawRanges(){
        if (rightRange.getX() < graphWidth + leftIndent){
            redrawRanges(leftRangeVal, rightRangeVal);
        } else {
            redrawRanges(convertRange(graphWidth + leftIndent - (rightRange.getX() - leftRange.getX())) ,
                    convertRange(graphWidth + leftIndent));
        }
    }

    public void redrawRanges(final float newMinTopX, final float newMaxTopX){
        leftRangeVal = newMinTopX;
        rightRangeVal = newMaxTopX;
        graphModel.bottomPlot.calculateMinMaxVals();
        minXVal = graphModel.bottomPlot.getCalculatedMinX().floatValue();
        maxXVal = graphModel.bottomPlot.getCalculatedMaxX().floatValue();
        if (null != graphModel.bottomPlot.getGraphWidget().getGridRect()){
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
            
            leftRangeVal = convertRange(leftRange.getX());
            rightRangeVal = convertRange(rightRange.getX());
            graphModel.setNewRanges(leftRangeVal, rightRangeVal);
            redrawAmputations();
            break;
        }
        return true;
    }
    
    public void setGraphModel(GraphModel graphModel) {
        this.graphModel = graphModel;
    }
}
