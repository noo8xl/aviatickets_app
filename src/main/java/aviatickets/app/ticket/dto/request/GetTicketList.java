package aviatickets.app.ticket.dto.request;

import java.time.LocalDate;

public record GetTicketList(
    LocalDate flightDate,
    String status // scheduled, active, landed, cancelled, incident, diverted

) {
}