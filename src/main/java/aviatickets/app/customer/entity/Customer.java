package aviatickets.app.customer.entity;

import java.sql.Date;
import java.util.Collection;
import java.util.List;

import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.Positive;
import lombok.Getter;
import org.hibernate.validator.constraints.Length;
import jdk.jfr.Timestamp;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.userdetails.UserDetails;


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
	@NotEmpty
	@Length(min = 8, max = 30)
	private String password;

	@Timestamp
	private Date createdAt = new Date(System.currentTimeMillis());
	@NotEmpty
	private Role role = Role.USER;
	@Getter
	private Boolean isBanned = false;
	@Getter
	private Boolean twoStepStatus = false;

	public Customer(){}


	public void setCustomer(
			Integer id, String name, String email, String password,
			Date createdAt, Role role, Boolean isBanned, Boolean twoStepStatus
	) {
		this.id = id;
		this.name = name;
		this.email = email;
		this.password = password;

		this.createdAt = createdAt;
		this.role = role;
		this.isBanned = isBanned;
		this.twoStepStatus = twoStepStatus;
	}

	public Customer getCustomer() {
		return this;
	}

	@Override
	public Collection<? extends GrantedAuthority> getAuthorities() {
		return List.of(new SimpleGrantedAuthority(this.role.name()));
	}

	// ############################# getters area #####################################

	@Override
	public String getPassword() {
		return this.password;
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


	@Override
	public boolean isAccountNonExpired() {
		return true;
	}

	@Override
	public boolean isAccountNonLocked() {
		return true;
	}

	@Override
	public boolean isCredentialsNonExpired() {
		return true;
	}

	@Override
	public boolean isEnabled() {
		return true;
	}
}
