package aviatickets.app.customer.entity;

import jakarta.validation.constraints.Positive;

public record CustomerDetails(
    @Positive Integer id,

    String name,
    String surname,
    String email,

    String documentId

) {

}
