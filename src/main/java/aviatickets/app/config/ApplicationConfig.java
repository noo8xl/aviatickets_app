package aviatickets.app.config;

import aviatickets.app.customer.CustomerInterface;
import aviatickets.app.customer.entity.Customer;
import lombok.RequiredArgsConstructor;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.AuthenticationProvider;
import org.springframework.security.authentication.dao.DaoAuthenticationProvider;
import org.springframework.security.config.annotation.authentication.configuration.AuthenticationConfiguration;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.crypto.password.PasswordEncoder;

import java.sql.SQLException;

@Configuration
@RequiredArgsConstructor
public class ApplicationConfig {

	private final CustomerInterface customerService;

	@Bean
	public UserDetailsService userDetailsService() {
		return username -> {
			try {
				Customer c = this.customerService.findOne(username);
				if (Boolean.TRUE.equals(c == null)) {
					throw new UsernameNotFoundException(username);
				} else return c.getCustomer();
			} catch (SQLException | ClassNotFoundException e) {
				throw new RuntimeException(e);
			}
		};
	}

	@Bean
	public AuthenticationProvider authenticationProvider() {
		DaoAuthenticationProvider authProvider = new DaoAuthenticationProvider();
		authProvider.setUserDetailsService(userDetailsService());
		authProvider.setPasswordEncoder(this.passwordEncoder());
		return authProvider;
	}

	@Bean
	public AuthenticationManager authenticationManager(AuthenticationConfiguration config) throws Exception {
		return config.getAuthenticationManager();
	}

	@Bean
	public PasswordEncoder passwordEncoder() {
		return new BCryptPasswordEncoder();
	}


}
