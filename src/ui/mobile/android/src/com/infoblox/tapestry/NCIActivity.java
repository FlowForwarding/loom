package com.infoblox.tapestry;

import android.net.Uri;
import android.os.Bundle;
import android.view.View;
import android.widget.AdapterView;
import android.widget.ListView;
import android.app.Activity;
import android.content.Intent;
import android.content.res.Resources;

public class NCIActivity extends Activity {

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_nci);
        ListView infoList = (ListView) findViewById(R.id.infolist);
        Resources res = getResources();
        
        String[] labels = {res.getString(R.string.about_nci),
                res.getString(R.string.nci_technical_paper),
                res.getString(R.string.about_flowforwarding)};
        
        final String[] urls = {res.getString(R.string.about_nci_url),
                res.getString(R.string.nci_technical_paper_url),
                res.getString(R.string.about_flowforwarding_url)};
        
        InfoListAdapter adapter = new InfoListAdapter(this, labels);
        infoList.setAdapter(adapter);
        infoList.setOnItemClickListener(new AdapterView.OnItemClickListener() {

            @Override
            public void onItemClick(AdapterView<?> arg0, View view, int position,
                    long id) {
               String url = urls[position];
               Intent browserIntent = new Intent(Intent.ACTION_VIEW, Uri.parse(url));
               startActivity(browserIntent);
            } 
            
        });
    }

    public void toggleInfoMenu(View v) {
        View list = findViewById(R.id.infolist);
        if (list.getVisibility() == View.VISIBLE){
            list.setVisibility(View.INVISIBLE);
        } else {
            list.setVisibility(View.VISIBLE);
        }
    }
}
