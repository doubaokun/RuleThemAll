package rta.plugin;

import rta.plugin.RemoteObject;

interface OnResetTimers {
    void onResetTimers(in List<RemoteObject> timers);
}
