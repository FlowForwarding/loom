package com.infoblox.tapestry;

import java.util.ArrayList;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.ImageView;
import android.widget.TextView;
import android.view.View.OnClickListener;

public class TapestryUrlsAdapter extends ArrayAdapter<String>{
    
    private final ArrayList<String> itemsArrayList;
    private LayoutInflater inflater;
    private Context context;
    
    public TapestryUrlsAdapter (Context context, ArrayList<String> itemsArrayList) {
        super(context, R.layout.urlitem, itemsArrayList);
        this.inflater =  (LayoutInflater) context
                .getSystemService(Context.LAYOUT_INFLATER_SERVICE);
        this.context = context;
        this.itemsArrayList = itemsArrayList;
    }
    
    @Override
    public View getView(final int position, View convertView, ViewGroup parent) {
        View rowView = this.inflater.inflate(R.layout.urlitem, parent, false);
        TextView urlLabel = (TextView) rowView.findViewById(R.id.tapestryurl);
        urlLabel.setText(itemsArrayList.get(position));
        final ImageView delaction = (ImageView) rowView.findViewById(R.id.deleteicon);
        if (position == 0 || position == 1){
            delaction.setVisibility(View.INVISIBLE);
        }

        final TapestryUrlsAdapter thisAdapter = this;   
        delaction.setOnClickListener(new OnClickListener(){
            @Override
            public void onClick(View v) {           
                itemsArrayList.remove(position);
                thisAdapter.notifyDataSetChanged();
                NCIActivity.saveTapestryUrls(thisAdapter.context, itemsArrayList);
            }
            
        });
        return rowView;
    }

}
