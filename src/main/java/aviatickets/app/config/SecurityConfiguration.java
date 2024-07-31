package aviatickets.app.config;

import aviatickets.app.jwt.JwtAuthFilter;
import lombok.RequiredArgsConstructor;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.authentication.AuthenticationProvider;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.web.configurers.AbstractHttpConfigurer;
import org.springframework.security.config.http.SessionCreationPolicy;
import org.springframework.security.web.SecurityFilterChain;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;
import org.springframework.security.web.util.matcher.AntPathRequestMatcher;

import static org.springframework.security.config.Customizer.withDefaults;

import aviatickets.app.customer.entity.Role;

@Configuration
@EnableWebSecurity
@RequiredArgsConstructor
public class SecurityConfiguration {

	private final JwtAuthFilter jwtAuthFilter;
	private final AuthenticationProvider authenticationProvider;

	private final String[] adminWhitelist = {


			// <- admin permission only **
			"/customer/update/update-ban-status/**",
			"/customer/get-customer-list/**",
			"/customer/delete/**",
			"/customer/create/**",

			"/flights/create-new-flight/",

			"/action/get-action-list/**",

			"/purchase/update-purchase-data/**"
	};

	private final String[] signedUserWhitelist = {

			// -> signed user only
			"/purchase/create/**",
			"/purchase/get-details/**",


	};


//	@Bean
//	public SecurityFilterChain securityFilterChain(HttpSecurity http) throws Exception {
//		http
//				.csrf(AbstractHttpConfigurer::disable)
//				.authorizeHttpRequests(auth -> auth
//						.requestMatchers(this.AUTH_WHITELIST)
//				);
////				.requestMatchers(this.AUTH_WHITELIST)
////				.permitAll()
////				.anyRequest()
////				.authenticated()
////				.and()
////				.sessionManagement()
////				.sessionCreationPolicy(SessionCreationPolicy.STATELESS)
////				.and()
////				.authenticationProvider(this.authenticationProvider)
////				.addFilterBefore(this.jwtAuthFilter, UsernamePasswordAuthenticationFilter.class);
//
//		return http.build();
//	}


	@Bean
	SecurityFilterChain securityFilterChain(HttpSecurity http) throws Exception {
		return http
				.csrf(AbstractHttpConfigurer::disable)
				.authorizeHttpRequests(auth -> auth
					.requestMatchers(this.signedUserWhitelist)
					.permitAll()
				)
				.authorizeHttpRequests(request -> request
					.requestMatchers(this.adminWhitelist)
					.hasRole(Role.ADMIN.toString())
					.anyRequest()
					.authenticated()
				)
				// .permitAll()
				// .anyRequest()
				// .authenticated()
				.sessionManagement(session -> session.sessionCreationPolicy(SessionCreationPolicy.STATELESS))
				.httpBasic(withDefaults())
				.authenticationProvider(this.authenticationProvider)
				.addFilterBefore(this.jwtAuthFilter, UsernamePasswordAuthenticationFilter.class)
				//.addFilterAfter(authenticationJwtTokenFilter, UsernamePasswordAuthenticationFilter.class)
				.build();
	}

//	@Bean
//	public SecurityFilterChain configure(HttpSecurity httpSecurity) throws Exception {
//		httpSecurity
//				.authorizeHttpRequests((requests) -> requests
//						.requestMatchers( new AntPathRequestMatcher("swagger-ui/**")).permitAll()
//						.requestMatchers( new AntPathRequestMatcher("/swagger-ui/**")).permitAll()
//						.requestMatchers( new AntPathRequestMatcher("v3/api-docs/**")).permitAll()
//						.requestMatchers( new AntPathRequestMatcher("/v3/api-docs/**")).permitAll()
//						.anyRequest().authenticated())
//				.httpBasic();
//		return httpSecurity.build();
//	}
}
