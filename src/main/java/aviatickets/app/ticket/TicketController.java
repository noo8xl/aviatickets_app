package aviatickets.app.ticket;

// import java.util.Date;
// import java.util.List;

// import org.springframework.http.HttpStatus;
// import org.springframework.web.bind.annotation.GetMapping;
// import org.springframework.web.bind.annotation.PostMapping;
// import org.springframework.web.bind.annotation.ResponseStatus;
// import aviatickets.app.ticket.entity.Ticket;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.bind.annotation.RequestMapping;

@RestController
@RequestMapping("/ticket")
public class TicketController {

  // public final TicketService ticketService;

  // public TicketController(TicketService ticketService) {
  // this.ticketService = ticketService;
  // }

  // @ResponseStatus(HttpStatus.OK)
  // @GetMapping("/get-hot/")
  // List<Ticket> getHotTicketList() {
  // return ticketService.getHotList(new Date());
  // }

  // @ResponseStatus(HttpStatus.OK)
  // @PostMapping("/find-ticket/")
  // Ticket findTicket() {
  // return ticketService.findTicketByFilter();
  // }

}
