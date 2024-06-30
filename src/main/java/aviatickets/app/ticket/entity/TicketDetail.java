package aviatickets.app.ticket.entity;

import java.util.Date;

import jakarta.validation.constraints.Positive;

public record TicketDetail(
    @Positive Integer id,

    Date departureTime,
    Date arrivalTime,

    @Positive Float price, // 00.00 format
    @Positive Integer discount, // value in %

    @Positive Integer ticketId,
    String ticketPath // path to the PDF ticket
) {

}
