export interface UserLogin {
  username: string;
  password: string;
}

export interface UserCreate {
  username: string;
  email: string;
  password: string;
  full_name?: string;
}

export interface Token {
  access_token: string;
  token_type: string;
}

export interface UserResponse {
  message: string;
  user: {
    id: string;
    username: string;
    email: string;
    full_name?: string;
    role: string;
    is_active: boolean;
    created_at: string;
    updated_at: string;
  }
}

export interface HTTPValidationError {
  detail: Array<{
    loc: Array<string | number>;
    msg: string;
    type: string;
  }>;
}