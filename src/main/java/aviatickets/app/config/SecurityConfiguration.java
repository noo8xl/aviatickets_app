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

import static org.springframework.security.config.Customizer.withDefaults;

@Configuration
@EnableWebSecurity
@RequiredArgsConstructor
public class SecurityConfiguration {

	private final JwtAuthFilter jwtAuthFilter;
	private final AuthenticationProvider authenticationProvider;

	private final String[] AUTH_WHITELIST = {

			// -> signed user only
			"/purchase/create/**",
			"/purchase/get-details/**",

			// <- admin permission only **
			"/customer/update/update-ban-status/**",
			"/customer/get-customer-list/**",
			"/customer/delete/**",
			"/customer/create/**",

			"/flights/create-new-flight/",

			"/action/get-action-list/**",

			"/purchase/update-purchase-data/**"
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
	public SecurityFilterChain securityFilterChain(HttpSecurity http) throws Exception {
		return http
				.csrf(AbstractHttpConfigurer::disable)
				.authorizeHttpRequests(auth -> auth
					.requestMatchers(this.AUTH_WHITELIST)
					.permitAll()
					.anyRequest()
					.authenticated()
				)
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
