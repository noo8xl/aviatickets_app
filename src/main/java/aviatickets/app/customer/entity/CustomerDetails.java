package aviatickets.app.customer.entity;

import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.Positive;

public record CustomerDetails(
    @Positive Integer id,

		@NotEmpty
    String name,
    String surname,
    String email,

    String documentId

) {

}
