package sta.tests;

import android.app.Activity;
import android.os.AsyncTask;
import android.widget.TextView;
import scala.Function0;

public class BenchmarkTask extends AsyncTask<Void, Void, String> {
    private final Activity root;
    private final int buttonID;
    private final Function0<String> action;

    public BenchmarkTask(Activity root, int buttonID, Function0<String> action) {
        this.root = root;
        this.buttonID = buttonID;
        this.action = action;
    }

    @Override
    protected String doInBackground(Void... params) {
        return action.apply();
    }

    @Override
    protected void onPreExecute() {
        root.findViewById(buttonID).setEnabled(false);
    }

    @Override
    protected void onPostExecute(String result) {
        ((TextView) root.findViewById(R.id.result)).setText(result);
        root.findViewById(buttonID).setEnabled(true);
    }
}
