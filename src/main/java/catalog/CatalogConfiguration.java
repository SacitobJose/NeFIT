package catalog;

import io.dropwizard.Configuration;

public class CatalogConfiguration extends Configuration {
    public String template;

    public String defaultName = "Stranger";

    public CatalogConfiguration(String template, String defaultName) {
        this.template = template;
        this.defaultName = defaultName;
    }
}

