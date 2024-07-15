package aviatickets.app.flight;

import java.util.Date;

import aviatickets.app.flight.dto.response.FlightList;

public interface FlightInteraction {

  public FlightList getHotFlightList();

  public FlightList getFlightList(String from, String to, Date d);

  public FlightList findFlightByFilter();

}
