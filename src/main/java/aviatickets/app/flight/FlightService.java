package aviatickets.app.flight;

import java.util.Date;

import org.springframework.stereotype.Service;

import aviatickets.app.flight.dto.response.FlightList;

@Service
public class FlightService implements FlightInteraction {

  public FlightList getHotFlightList() {
    // get cheepest today flight list new Date()
    return null;
  }

  public FlightList getFlightList(String from, String to, Date d) {
    return null;
  }

  public FlightList findFlightByFilter() {
    return null;
  }

}
