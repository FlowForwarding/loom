package com.infoblox.tapestry;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.TextView;

public class InfoListAdapter extends ArrayAdapter<String> {

    private final String[] itemsArrayList;
    private LayoutInflater inflater;

    public InfoListAdapter(Context context, String[] itemsArrayList) {
        super(context, R.layout.infoitem, itemsArrayList);
        this.itemsArrayList = itemsArrayList;
        this.inflater = (LayoutInflater) context
        .getSystemService(Context.LAYOUT_INFLATER_SERVICE);
    }

    @Override
    public View getView(int position, View convertView, ViewGroup parent) {
        View rowView = this.inflater.inflate(R.layout.infoitem, parent, false);
        TextView labelView = (TextView) rowView.findViewById(R.id.infotext);
        labelView.setText(itemsArrayList[position]);
        return rowView;
    }

}
