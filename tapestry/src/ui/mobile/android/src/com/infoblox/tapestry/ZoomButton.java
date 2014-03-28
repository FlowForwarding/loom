package com.infoblox.tapestry;

import android.content.Context;
import android.content.res.TypedArray;
import android.util.AttributeSet;
import android.widget.TextView;

public class ZoomButton extends TextView{
    
    private int period;
    private boolean disabled;

    public int getPeriod() {
        return period;
    }
    
    public boolean isDisabled(){
        return disabled;
    }

    public ZoomButton(Context context, AttributeSet attrs) {
        super(context, attrs);
        TypedArray a = context.getTheme().obtainStyledAttributes(
                attrs,
                R.styleable.ZoomButton,
                0, 0);

           try {
               period = a.getInt(R.styleable.ZoomButton_period, 0);
           } finally {
               a.recycle();
           }
    }
    
    public void deselect(){
        this.setBackgroundColor(getResources().getColor(R.color.white));
        this.setTextColor(getResources().getColor(R.color.black));
    }
    
    public void select(){
        this.setBackgroundColor(getResources().getColor(R.color.black));
        this.setTextColor(getResources().getColor(R.color.white));
    }
    
    public void reset(){
        this.setBackgroundColor(getResources().getColor(R.color.white));
        this.setTextColor(getResources().getColor(R.color.black));
        disabled = false;
    }
    
    public void setDisabled(){
        disabled = true;
        this.setBackgroundColor(getResources().getColor(R.color.tapestryrangesgrey));
    }
    
}