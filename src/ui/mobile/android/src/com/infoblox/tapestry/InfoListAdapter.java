package com.infoblox.tapestry;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.TextView;

public class InfoListAdapter extends ArrayAdapter<String> {

    private final Context context;
    private final String[] itemsArrayList;

    public InfoListAdapter(Context context, String[] itemsArrayList) {
        super(context, R.layout.infoitem, itemsArrayList);
        this.context = context;
        this.itemsArrayList = itemsArrayList;
    }

    @Override
    public View getView(int position, View convertView, ViewGroup parent) {
        LayoutInflater inflater = (LayoutInflater) context
                .getSystemService(Context.LAYOUT_INFLATER_SERVICE);
        View rowView = inflater.inflate(R.layout.infoitem, parent, false);
        TextView labelView = (TextView) rowView.findViewById(R.id.infotext);
        labelView.setText(itemsArrayList[position]);
        return rowView;
    }

}
