package aviatickets.app.config;

import aviatickets.app.jwt.JwtAuthFilter;
import lombok.RequiredArgsConstructor;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.web.configurers.AbstractHttpConfigurer;
import org.springframework.security.config.http.SessionCreationPolicy;
import org.springframework.security.web.SecurityFilterChain;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;

@Configuration
@EnableWebSecurity
@RequiredArgsConstructor
public class SecurityConfiguration {

	private final JwtAuthFilter jwtAuthFilter;

	// <- authless routes
	private final String[] authlessRoutes = {
			"/auth/**"
	};

	// <- admin permission only **
	private final String[] adminWhitelist = {


			"/customer/update/update-ban-status/**",
			"/customer/get-customer-list/**",
			"/customer/delete/**",
			"/customer/create/**",

			"/flights/create-new-flight/",

			"/action/get-action-list/**",

			"/purchase/update-purchase-data/**"
	};

	// -> signed user only
	private final String[] signedUserWhitelist = {

			"/purchase/create/**",
			"/purchase/get-details/**",
			"/get-customer-by-id/**",
			"/get-customer-by-email/**"

	};


	@Bean
	SecurityFilterChain securityFilterChain(HttpSecurity http) throws Exception {

		http.csrf(AbstractHttpConfigurer::disable)
				.authorizeHttpRequests( auth -> auth
					.requestMatchers(this.authlessRoutes).permitAll()  // Allow access to all without authentication
					.requestMatchers(this.adminWhitelist).hasRole("ADMIN")  // Allow only ADMIN role
					.requestMatchers(this.signedUserWhitelist).hasAnyRole("USER", "ADMIN")  // Allow USER and ADMIN roles
					.anyRequest().authenticated()
				)
				.sessionManagement(session -> session
					.sessionCreationPolicy(SessionCreationPolicy.STATELESS)
				);

		http.addFilterBefore(this.jwtAuthFilter, UsernamePasswordAuthenticationFilter.class);

		return http.build();
	}


}
