package aviatickets.app.ticket.entity.flight;

import java.time.LocalDateTime;

public record Live(
    LocalDateTime updated,
    Long latitude,
    Long longtitude,
    Float altitude,
    Float direction,
    Float speedHorizontal,
    Float speedVertical,
    Boolean isGround) {

}
