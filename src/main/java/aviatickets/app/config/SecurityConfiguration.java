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
			"/auth/**",
			"/flights/get-hot/**",
			"/flights/find-filtered-flight/**",
			"/h2-console/**",


			"/purchase/**",


//			"/flights/create-new-flight/", // -> test only
	};

	// <- administrator permission only **
	private final String[] adminWhitelist = {

			"/customer/update-ban-status/**",
			"/customer/get-customer-list/**",
			"/customer/delete/**",
			"/customer/create/**",

			"/action/get-action-list/**",

			"/flights/create-new-flight/",

//			"/purchase/update/**",
//			"/purchase/get-purchase-list/**",
//			"/purchase/get-all/**",
//			"/purchase/delete/**",

	};

	// -> signed user only
	private final String[] signedUserWhitelist = {

			"/customer/get-customer-by-id/**",
			"/customer/get-customer-by-email/**",
			"/customer/update/**",


			"/purchase/create/**",
			"/purchase/get-details-by-id/**",
			"/purchase/get-history/**",

	};


	@Bean
	SecurityFilterChain securityFilterChain(HttpSecurity http) throws Exception {

		http.csrf(AbstractHttpConfigurer::disable)
				.authorizeHttpRequests( auth -> auth
					.requestMatchers(this.authlessRoutes).permitAll()  // Allow access to all without authentication
					.requestMatchers(this.adminWhitelist).hasAuthority("ADMIN")  // Allow only ADMIN role
					.requestMatchers(this.signedUserWhitelist).hasAnyAuthority("USER", "ADMIN")  // Allow USER and ADMIN roles
					.anyRequest().authenticated()
				)
				.sessionManagement(session -> session
					.sessionCreationPolicy(SessionCreationPolicy.STATELESS)
				);

		http.addFilterBefore(this.jwtAuthFilter, UsernamePasswordAuthenticationFilter.class);

		return http.build();
	}

}
