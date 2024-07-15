package aviatickets.app.ticket.entity;

import aviatickets.app.customer.entity.CustomerDetails;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.Positive;

// Ticket -> is a ticket entity which customer already bought 
public record Ticket(
		@Positive Integer id,
		@NotEmpty String destinanion,
		String flightId,

		String companyName,
		TicketDetail details,
		CustomerDetails customer) {

}

// https://aviationstack.com/documentation <-
