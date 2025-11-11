export interface AdminUser {
  userId: string;
  authenticationStatus: boolean;
  createdAt: string;
  updatedAt: string;
}

export interface CreateAdminUserRequest {
  userId: string;
  authenticationStatus: boolean;
}

export interface UpdateAdminUserRequest {
  authenticationStatus?: boolean;
}
