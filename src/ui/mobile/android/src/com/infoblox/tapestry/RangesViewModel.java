package com.infoblox.tapestry;

import com.androidplot.xy.XYPlot;

import android.app.Activity;
import android.view.View;

public class RangesViewModel {
    
    private View leftRange;
    private View rightRange;
    private View leftAmputation;
    private View rightAmputation;
    
    private float minXVal;
    private float maxXVal;
    private float graphWidth;
    
    public RangesViewModel(Activity activity){
        leftRange = activity.findViewById(R.id.leftRange);
        rightRange = activity.findViewById(R.id.rightRange);
        leftAmputation = activity.findViewById(R.id.leftAmputation);
        rightAmputation = activity.findViewById(R.id.rightAmputation);
    }
    
    public void initDimensions(XYPlot plot){
        minXVal = plot.getCalculatedMinX().floatValue();
        maxXVal = plot.getCalculatedMaxX().floatValue();
        graphWidth = plot.getGraphWidget().getGridRect().width();
    }

    public void redrawRanges(float newMinX, float newMaxX){
        leftRange.setX ((newMinX- minXVal)*graphWidth/(maxXVal - minXVal));
        rightRange.setX (graphWidth - (maxXVal - newMaxX)*graphWidth/(maxXVal - minXVal));
    }
}
