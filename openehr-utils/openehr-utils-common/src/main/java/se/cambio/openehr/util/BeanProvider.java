package se.cambio.openehr.util;

import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import se.cambio.openehr.util.configuration.SpringConfiguration;

public class BeanProvider {

    private static BeanProvider instance;
    public ConfigurableApplicationContext appCtx;

    private BeanProvider() {
    }

    private ConfigurableApplicationContext getAppCtx() {
        if (appCtx == null) {
            appCtx = new AnnotationConfigApplicationContext();
            appCtx.getEnvironment().setDefaultProfiles("cm-admin-plain-service", "terminology-plain-service", "cm-admin-file-dao");
            ((AnnotationConfigApplicationContext)appCtx).register(SpringConfiguration.class);
            appCtx.refresh();
        }
        return appCtx;
    }

    public static <E> E getBean(Class<E> beanClass) {
        return getInstance().getAppCtx().getBean(beanClass);
    }

    public static void setActiveProfiles (String... activeProfiles) {
        getInstance().appCtx = new AnnotationConfigApplicationContext();
        getInstance().appCtx.getEnvironment().setActiveProfiles(activeProfiles);
        ((AnnotationConfigApplicationContext)getInstance().appCtx).register(SpringConfiguration.class);
        getInstance().appCtx.refresh();
    }

    public static String[] getActiveProfiles() {
        return getInstance().getAppCtx().getEnvironment().getActiveProfiles();
    }

    public static void setApplicationContext(ConfigurableApplicationContext appCtx) {
        getInstance().appCtx = appCtx;
    }

    public static BeanProvider getInstance() {
        if (instance == null) {
            instance = new BeanProvider();
        }
        return instance;
    }
}
