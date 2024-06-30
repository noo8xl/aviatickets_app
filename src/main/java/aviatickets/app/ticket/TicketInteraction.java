package aviatickets.app.ticket;

import java.util.Date;
import java.util.List;

public interface TicketInteraction {

  public List<Integer> getTicketList(String from, String to, Date date);

  public List<Integer> getCompanyList();

  public Integer getCompatyDetails(String companyName, Integer companyId);
  // public List<Integer>
  // public List<Integer>
  // public List<Integer>
  // public List<Integer>

}
