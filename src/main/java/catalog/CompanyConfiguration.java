package catalog;

import io.dropwizard.Configuration;
import org.hibernate.validator.constraints.NotEmpty;

public class CompanyConfiguration extends Configuration {
    @NotEmpty
    public String template;

    public String defaultName = "Stranger";

    public CompanyConfiguration(String template, String defaultName) {
        this.template = template;
        this.defaultName = defaultName;
    }
}

