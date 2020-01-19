package catalog;

import io.dropwizard.Application;
import io.dropwizard.setup.Bootstrap;
import io.dropwizard.setup.Environment;

import catalog.health.TemplateHealthCheck;

public class CatalogApplication extends Application<CatalogConfiguration> {
    public static void main(String[] args) throws Exception {
        new CatalogApplication().run(args);
    }

    @Override
    public String getName() { return "Catalog"; }

    @Override
    public void initialize(Bootstrap<CatalogConfiguration> bootstrap) { }

    @Override
    public void run(CatalogConfiguration configuration,
                    Environment environment) {
        environment.jersey().register(
            new CatalogConfiguration(configuration.template, configuration.defaultName));
        environment.healthChecks().register("template",
            new TemplateHealthCheck(configuration.template));
    }

}

