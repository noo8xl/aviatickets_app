package aviatickets.app.actions.entity;

import com.fasterxml.jackson.annotation.JsonIgnore;
import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.Positive;
import jdk.jfr.Timestamp;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.sql.Date;


@NoArgsConstructor
public class ActionLog {

	@Positive
	@Setter
	private Integer id;
	@Getter
	@NotEmpty
	@Email
	private String email;

@Setter
	@Timestamp
	private Date date = new Date(System.currentTimeMillis());

	@Getter
	@NotEmpty
	private String action;
	@Getter
	@Positive
	private Integer customerId;

public void setAction(Integer id, String email, Date date, String action, Integer customerId) {
		this.id = id;
		this.email = email;
		this.action = action;
		this.customerId = customerId;

		if(Boolean.TRUE.equals(date != null)) {
			this.date = date;
		}
	}

	@JsonIgnore
	public ActionLog getActionEntity() {
		return this;
	}

}