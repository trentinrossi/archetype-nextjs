// Authentication types for User and Security Administration

export interface LoginRequest {
  userId: string;
  password: string;
}

export interface LoginResponse {
  userId: string;
  firstName: string;
  lastName: string;
  userType: 'A' | 'R';
  message: string;
}

export interface AuthUser {
  userId: string;
  firstName: string;
  lastName: string;
  userType: 'A' | 'R';
}

export interface AuthState {
  user: AuthUser | null;
  isAuthenticated: boolean;
  isLoading: boolean;
}

export interface UserResponse {
  userId: string;
  firstName: string;
  lastName: string;
  userType: 'A' | 'R';
}

export interface UserCreate {
  username: string;
  password: string;
  email: string;
  fullName: string;
}
