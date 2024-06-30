package aviatickets.app.ticket;

import java.util.List;

import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/avia")
public class TicketController {

  public final TicketService ticketService;

  public TicketController(TicketService ticketService){
    this.ticketService = ticketService;
  }

  @ResponseStatus(HttpStatus.CREATED)
  @PostMapping("/getTicketList/")
  List<Integer> getTicketList() {
    
    return ticketService.getTicketList("Milan", "Amsterdam", 03123717);
  }


}
