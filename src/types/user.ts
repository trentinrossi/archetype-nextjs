export interface User {
  id: number;
  userType: string;
  userTypeDisplay: string;
  authenticated: boolean;
  authenticatedDisplay: string;
  userId: string;
  firstName: string;
  lastName: string;
  fullName: string;
  isAdmin: boolean;
  createdAt: string;
  updatedAt: string;
}

export interface CreateUserRequest {
  userType: string;
  authenticated: boolean;
  userId: string;
  password: string;
  firstName: string;
  lastName: string;
}

export interface UpdateUserRequest {
  userType?: string;
  authenticated?: boolean;
  password?: string;
  firstName?: string;
  lastName?: string;
}

export interface LoginRequest {
  userId: string;
  password: string;
}

export interface LoginResponse {
  success: boolean;
  message: string;
  user: {
    id: number;
    userId: string;
    userType: string;
    firstName: string;
    lastName: string;
    fullName: string;
    authenticated: boolean;
  };
  sessionToken: string;
}
