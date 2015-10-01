package sta.plugin;

import sta.plugin.OnNewModel;
import sta.plugin.OnResetTimers;
import sta.plugin.RemoteObject;

interface IPlugin {
    void register(in OnNewModel onNewModel, in OnResetTimers onResetTimers);

    RemoteObject actionParser();

    RemoteObject triggerParser();

    RemoteObject executeAction(in RemoteObject action);
}
