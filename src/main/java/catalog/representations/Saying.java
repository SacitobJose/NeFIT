package catalog.representations;

//import com.fasterxml.jackson.annotation.*;

public class Saying {
    public long id;
    public String name;
    public String address;

    public Saying(long id, String name) {
        this.id = id;
        this.name = name;
    }
}
