package aviatickets.app.customer.entity;

import com.fasterxml.jackson.annotation.JsonIgnore;
import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.Positive;
import jdk.jfr.Timestamp;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.hibernate.validator.constraints.Length;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.userdetails.UserDetails;

import java.sql.Date;
import java.util.Collection;
import java.util.List;


@Getter
@NoArgsConstructor
public class Customer implements UserDetails {
	@Getter
	@Positive
	private Integer id;

	@Getter
	@NotEmpty
	@Length(min = 4, max = 28)
	private String name;
	@NotEmpty
	@Email
	private String email;
	@Getter
	@NotEmpty
	@Length(min = 8, max = 30)
	private String password;

	@Timestamp
	@Setter
	private Date createdAt = new Date(System.currentTimeMillis());
	@Timestamp
	@Setter
	private Date updatedAt = new Date(System.currentTimeMillis());
	@NotEmpty
	@Setter
	private Role role = Role.USER;
	@Getter
	private Boolean isBanned = false;
	@Getter
	private Boolean twoStepStatus = false;


	public void setCustomer(
			Integer id, String name, String email, String password,
			Boolean isBanned, Boolean twoStepStatus
	) {
		if (Boolean.FALSE.equals(id == null))
			this.id = id;

		this.name = name;
		this.email = email;
		this.password = password;

		if (Boolean.FALSE.equals(isBanned == null))
			this.isBanned = isBanned;

		if (Boolean.FALSE.equals(twoStepStatus == null))
			this.twoStepStatus = twoStepStatus;

	}

// ############################# getters area #####################################


	@JsonIgnore
	public Customer getCustomer() {
		return this;
	}

	@Override
	public Collection<? extends GrantedAuthority> getAuthorities() {
		return List.of(new SimpleGrantedAuthority(this.role.name()));
	}

	@Override
	public String getUsername() {
		return this.email;
	}

//	@Override
//	public UserDetails loadUserByUsername(String username) throws UsernameNotFoundException {
//		if(this.role.equals(Role.USER)) {
//			List<GrantedAuthority> grantedAuthorities = new ArrayList<>();
//			grantedAuthorities.add(new SimpleGrantedAuthority(this.role.name()));
//		} else if(this.role.equals(Role.ADMIN)) {
//
//		} else {
//			throw new UsernameNotFoundException(username);
//		}
//	}

// ########################### end of getters area ##################################
}
