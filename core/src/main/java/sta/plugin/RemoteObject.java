package sta.plugin;

import android.os.Parcel;
import android.os.Parcelable;
import java.io.Serializable;

public class RemoteObject implements Parcelable {
  private final Serializable object;

  public RemoteObject(Serializable object) {
    this.object = object;
  }

  public int describeContents() {
    return 0;
  }

  public void writeToParcel(Parcel dest, int flags) {
    dest.writeSerializable(object);
  }

  public static final Parcelable.Creator<RemoteObject> CREATOR = new Parcelable.Creator<RemoteObject>() {
    public RemoteObject createFromParcel(Parcel source) {
      return new RemoteObject(source.readSerializable());
    }

    public RemoteObject[] newArray(int size) {
      return new RemoteObject[size];
    }
  };

  @SuppressWarnings("unchecked")
  public <T> T as() {
    return (T) object;
  }
}
