package rta.plugin;

import rta.plugin.OnNewModel;
import rta.plugin.OnResetTimers;
import rta.plugin.RemoteObject;

interface IPlugin {
    void register(in OnNewModel onNewModel, in OnResetTimers onResetTimers);

    RemoteObject actionParser();

    RemoteObject triggerParser();

    RemoteObject executeAction(in RemoteObject action);
}
