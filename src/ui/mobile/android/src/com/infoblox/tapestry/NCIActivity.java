package com.infoblox.tapestry;

import android.os.Bundle;
import android.app.Activity;
import android.view.Menu;

public class NCIActivity extends Activity {

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_nci);
    }

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        // Inflate the menu; this adds items to the action bar if it is present.
        getMenuInflater().inflate(R.menu.nci, menu);
        return true;
    }

}
