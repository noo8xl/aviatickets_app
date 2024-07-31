package aviatickets.app.jwt;

import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.web.authentication.WebAuthenticationDetailsSource;
import org.springframework.stereotype.Component;
import org.springframework.web.filter.OncePerRequestFilter;

import java.io.IOException;

@Component
public class JwtAuthFilter extends OncePerRequestFilter {

	private final JwtService jwtService;
	private final UserDetailsService userDetailsService;

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

	public JwtAuthFilter(JwtService jwtService, UserDetailsService userDetailsService) {
		this.jwtService = jwtService;
		this.userDetailsService = userDetailsService;
	}

	@Override
	protected void doFilterInternal(
			HttpServletRequest request,
			HttpServletResponse response,
			FilterChain filterChain
		) throws ServletException, IOException {

			final String authHeader = request.getHeader("Authorization");
			final String path = request.getRequestURI();
			final String jwt;
			final String customerEmail;


			if (authHeader == null ) {
				filterChain.doFilter(request, response);
				return;
			}
			if(Boolean.FALSE.equals(authHeader.startsWith("Bearer "))) {
				filterChain.doFilter(request, response);
				return;
			}

			jwt = authHeader.substring(7);
			customerEmail = this.jwtService.extractCustomerEmail(jwt);

			if(customerEmail != null && SecurityContextHolder.getContext().getAuthentication() == null) {
				UserDetails userDetails = this.userDetailsService.loadUserByUsername(customerEmail);

				if(Boolean.TRUE.equals(this.jwtService.isTokenValid(jwt, userDetails))) {
					UsernamePasswordAuthenticationToken authToken = new UsernamePasswordAuthenticationToken(
						userDetails,
						null,
						userDetails.getAuthorities()
					);
					authToken.setDetails(new WebAuthenticationDetailsSource().buildDetails(request));
					SecurityContextHolder.getContext().setAuthentication(authToken);
				}
			}

		filterChain.doFilter(request, response);
	}
}
