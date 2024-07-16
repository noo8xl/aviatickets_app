package aviatickets.app.flight;

import java.time.LocalDateTime;
import java.util.List;

import org.springframework.cache.annotation.Cacheable;
import org.springframework.jdbc.core.simple.JdbcClient;
import org.springframework.stereotype.Repository;

import aviatickets.app.flight.entity.FlightsItem;

@Repository
public class FlightRepository {

  private final JdbcClient jdbcClient;

  public FlightRepository(JdbcClient jdbcClient) {
    this.jdbcClient = jdbcClient;
  }

  @Cacheable("hotFlights")
  public List<FlightsItem> getHotFlights() {
    String sqlStr = "SELECT * FROM flights WHERE departure_time=?"; // as example

    return jdbcClient.sql(sqlStr)
        .param(LocalDateTime.now())
        .query(FlightsItem.class)
        .list();
  }

  public List<FlightsItem> findFlightsByFilter() {

    String sqlStr = "SELECT ... ";

    return jdbcClient.sql(sqlStr)
        .params()
        .query(FlightsItem.class)
        .list();
  }

}
