package se.cambio.cds.util;

import org.apache.log4j.Appender;
import org.apache.log4j.Layout;
import org.apache.log4j.SimpleLayout;
import org.apache.log4j.spi.ErrorHandler;
import org.apache.log4j.spi.Filter;
import org.apache.log4j.spi.LoggingEvent;

public class WarningHandlerAppender implements Appender{

    private Layout _layout = null;

    @Override
    public void addFilter(Filter newFilter) {
	// TODO Auto-generated method stub

    }

    @Override
    public Filter getFilter() {
	// TODO Auto-generated method stub
	return null;
    }

    @Override
    public void clearFilters() {
	// TODO Auto-generated method stub

    }

    @Override
    public void close() {
	// TODO Auto-generated method stub

    }

    @Override
    public void doAppend(LoggingEvent event) {
	String msg = getLayout().format(event);
	if (msg!=null){
	    //TODO Senb by e-mail to admin?
	    System.out.print(msg);
	}else{
	    System.out.println(new SimpleLayout().format(event));
	}
    }

    @Override
    public String getName() {
	return "ThinkHubAppender";
    }

    @Override
    public void setErrorHandler(ErrorHandler errorHandler) {
	// TODO Auto-generated method stub

    }

    @Override
    public ErrorHandler getErrorHandler() {
	// TODO Auto-generated method stub
	return null;
    }

    @Override
    public void setLayout(Layout layout) {
	_layout = layout;
    }

    @Override
    public Layout getLayout() {
	if (_layout==null){
	    _layout = new SimpleLayout();
	}
	return _layout;
    }

    @Override
    public void setName(String name) {
	// TODO Auto-generated method stub

    }

    @Override
    public boolean requiresLayout() {
	// TODO Auto-generated method stub
	return false;
    }


}
