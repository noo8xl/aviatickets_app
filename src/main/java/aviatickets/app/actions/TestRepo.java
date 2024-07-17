//import aviatickets.app.flight.entity.FlightsItem;
//import org.springframework.data.jdbc.repository.query.Query;
//import org.springframework.data.repository.ListCrudRepository;
//
//import java.util.List;
//
//public interface TestRepo extends ListCrudRepository<FlightsItem, Integer> {
//
//    @Query("select * from flights WHERE airline=some")
//    List<FlightsItem> findAllByAirline(String airline);
//
//}