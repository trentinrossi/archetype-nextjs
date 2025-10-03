'use client';

import { useState, useCallback, useRef, useEffect } from 'react';
import { userService } from '@/services/userService';
import {
  UserResponse,
  UserListResponse,
  CreateUserRequest,
  UpdateUserRequest,
  UserSearchRequest,
  PaginationInfo,
  DEFAULT_PAGINATION,
} from '@/types/user';

interface UseUsersState {
  users: UserResponse[];
  loading: boolean;
  error: string | null;
  pagination: PaginationInfo | null;
  searchParams: UserSearchRequest;
}

interface UseUsersActions {
  fetchUsers: (params?: UserSearchRequest) => Promise<void>;
  createUser: (userData: CreateUserRequest) => Promise<UserResponse>;
  updateUser: (userId: string, userData: UpdateUserRequest) => Promise<UserResponse>;
  deleteUser: (userId: string) => Promise<void>;
  searchUsers: (searchTerm: string, params?: UserSearchRequest) => Promise<void>;
  refreshUsers: () => Promise<void>;
  setPage: (page: number) => Promise<void>;
  setLimit: (limit: number) => Promise<void>;
  setSortBy: (sortBy: keyof UserResponse, sortOrder?: 'asc' | 'desc') => Promise<void>;
  clearError: () => void;
  reset: () => void;
  bulkDeleteUsers: (userIds: string[]) => Promise<{ deleted: string[]; failed: string[] }>;
  checkUserExists: (userId: string) => Promise<boolean>;
}

interface UseUsersReturn extends UseUsersState, UseUsersActions {
  isFirstLoad: boolean;
  hasUsers: boolean;
  totalUsers: number;
  currentPage: number;
  totalPages: number;
  hasNextPage: boolean;
  hasPreviousPage: boolean;
  isSearching: boolean;
}

const initialState: UseUsersState = {
  users: [],
  loading: false,
  error: null,
  pagination: null,
  searchParams: { ...DEFAULT_PAGINATION },
};

export function useUsers(initialParams?: UserSearchRequest): UseUsersReturn {
  const [state, setState] = useState<UseUsersState>({
    ...initialState,
    searchParams: { ...DEFAULT_PAGINATION, ...initialParams },
  });

  const [isFirstLoad, setIsFirstLoad] = useState(true);
  const abortControllerRef = useRef<AbortController | null>(null);
  const lastFetchParamsRef = useRef<string>('');

  // Helper function to update state
  const updateState = useCallback((updates: Partial<UseUsersState>) => {
    setState(prev => ({ ...prev, ...updates }));
  }, []);

  // Helper function to handle errors
  const handleError = useCallback((error: unknown, defaultMessage: string) => {
    const errorMessage = error instanceof Error ? error.message : defaultMessage;
    updateState({ error: errorMessage, loading: false });
    console.error(defaultMessage, error);
    return errorMessage;
  }, [updateState]);

  // Clear error
  const clearError = useCallback(() => {
    updateState({ error: null });
  }, [updateState]);

  // Reset state
  const reset = useCallback(() => {
    // Cancel any ongoing requests
    if (abortControllerRef.current) {
      abortControllerRef.current.abort();
    }
    
    setState({
      ...initialState,
      searchParams: { ...DEFAULT_PAGINATION, ...initialParams },
    });
    setIsFirstLoad(true);
    lastFetchParamsRef.current = '';
  }, [initialParams]);

  // Fetch users with pagination and search
  const fetchUsers = useCallback(async (params?: UserSearchRequest) => {
    try {
      // Cancel previous request
      if (abortControllerRef.current) {
        abortControllerRef.current.abort();
      }

      const searchParams = { ...state.searchParams, ...params };
      const paramsKey = JSON.stringify(searchParams);

      // Avoid duplicate requests
      if (paramsKey === lastFetchParamsRef.current && !isFirstLoad) {
        return;
      }

      updateState({ loading: true, error: null, searchParams });
      lastFetchParamsRef.current = paramsKey;

      // Create new abort controller
      abortControllerRef.current = new AbortController();

      const response: UserListResponse = await userService.listUsers(searchParams);

      // Check if request was aborted
      if (abortControllerRef.current.signal.aborted) {
        return;
      }

      updateState({
        users: response.users || [],
        pagination: response.pagination,
        loading: false,
        searchParams,
      });

      setIsFirstLoad(false);
    } catch (error) {
      if (error instanceof Error && error.name === 'AbortError') {
        return; // Request was cancelled, don't update state
      }
      handleError(error, 'Failed to fetch users');
    }
  }, [state.searchParams, isFirstLoad, updateState, handleError]);

  // Create user
  const createUser = useCallback(async (userData: CreateUserRequest): Promise<UserResponse> => {
    try {
      updateState({ loading: true, error: null });

      const response = await userService.createUser(userData);
      const newUser = response.user;

      // Add new user to the beginning of the list
      updateState({
        users: [newUser, ...state.users],
        loading: false,
      });

      // Update pagination if available
      if (state.pagination) {
        updateState({
          pagination: {
            ...state.pagination,
            totalItems: state.pagination.totalItems + 1,
          },
        });
      }

      return newUser;
    } catch (error) {
      handleError(error, 'Failed to create user');
      throw error;
    }
  }, [state.users, state.pagination, updateState, handleError]);

  // Update user
  const updateUser = useCallback(async (userId: string, userData: UpdateUserRequest): Promise<UserResponse> => {
    try {
      updateState({ loading: true, error: null });

      const response = await userService.updateUser(userId, userData);
      const updatedUser = response.user;

      // Update user in the list
      const updatedUsers = state.users.map(user =>
        user.userId === userId ? updatedUser : user
      );

      updateState({
        users: updatedUsers,
        loading: false,
      });

      return updatedUser;
    } catch (error) {
      handleError(error, 'Failed to update user');
      throw error;
    }
  }, [state.users, updateState, handleError]);

  // Delete user
  const deleteUser = useCallback(async (userId: string): Promise<void> => {
    try {
      updateState({ loading: true, error: null });

      await userService.deleteUser(userId);

      // Remove user from the list
      const filteredUsers = state.users.filter(user => user.userId !== userId);

      updateState({
        users: filteredUsers,
        loading: false,
      });

      // Update pagination if available
      if (state.pagination) {
        updateState({
          pagination: {
            ...state.pagination,
            totalItems: Math.max(0, state.pagination.totalItems - 1),
          },
        });
      }
    } catch (error) {
      handleError(error, 'Failed to delete user');
      throw error;
    }
  }, [state.users, state.pagination, updateState, handleError]);

  // Bulk delete users
  const bulkDeleteUsers = useCallback(async (userIds: string[]): Promise<{ deleted: string[]; failed: string[] }> => {
    try {
      updateState({ loading: true, error: null });

      const result = await userService.bulkDeleteUsers(userIds);

      // Remove successfully deleted users from the list
      const filteredUsers = state.users.filter(user => !result.deleted.includes(user.userId));

      updateState({
        users: filteredUsers,
        loading: false,
      });

      // Update pagination if available
      if (state.pagination && result.deleted.length > 0) {
        updateState({
          pagination: {
            ...state.pagination,
            totalItems: Math.max(0, state.pagination.totalItems - result.deleted.length),
          },
        });
      }

      return result;
    } catch (error) {
      handleError(error, 'Failed to delete users');
      throw error;
    }
  }, [state.users, state.pagination, updateState, handleError]);

  // Search users
  const searchUsers = useCallback(async (searchTerm: string, params?: UserSearchRequest): Promise<void> => {
    const searchParams = {
      ...state.searchParams,
      ...params,
      searchTerm: searchTerm.trim(),
      page: 1, // Reset to first page when searching
    };

    await fetchUsers(searchParams);
  }, [state.searchParams, fetchUsers]);

  // Refresh users (re-fetch with current params)
  const refreshUsers = useCallback(async (): Promise<void> => {
    lastFetchParamsRef.current = ''; // Force refresh
    await fetchUsers(state.searchParams);
  }, [state.searchParams, fetchUsers]);

  // Set page
  const setPage = useCallback(async (page: number): Promise<void> => {
    if (page < 1) return;
    if (state.pagination && page > state.pagination.totalPages) return;

    await fetchUsers({ ...state.searchParams, page });
  }, [state.searchParams, state.pagination, fetchUsers]);

  // Set limit
  const setLimit = useCallback(async (limit: number): Promise<void> => {
    if (limit < 1 || limit > 100) return;

    await fetchUsers({
      ...state.searchParams,
      limit,
      page: 1, // Reset to first page when changing limit
    });
  }, [state.searchParams, fetchUsers]);

  // Set sort
  const setSortBy = useCallback(async (sortBy: keyof UserResponse, sortOrder: 'asc' | 'desc' = 'asc'): Promise<void> => {
    await fetchUsers({
      ...state.searchParams,
      sortBy,
      sortOrder,
      page: 1, // Reset to first page when changing sort
    });
  }, [state.searchParams, fetchUsers]);

  // Check if user exists
  const checkUserExists = useCallback(async (userId: string): Promise<boolean> => {
    try {
      return await userService.checkUserExists(userId);
    } catch (error) {
      console.error('Failed to check user existence:', error);
      return false;
    }
  }, []);

  // Computed values
  const hasUsers = state.users.length > 0;
  const totalUsers = state.pagination?.totalItems || 0;
  const currentPage = state.pagination?.currentPage || 1;
  const totalPages = state.pagination?.totalPages || 1;
  const hasNextPage = state.pagination?.hasNextPage || false;
  const hasPreviousPage = state.pagination?.hasPreviousPage || false;
  const isSearching = Boolean(state.searchParams.searchTerm?.trim());

  // Cleanup on unmount
  useEffect(() => {
    return () => {
      if (abortControllerRef.current) {
        abortControllerRef.current.abort();
      }
    };
  }, []);

  // Auto-fetch on mount if no initial data
  useEffect(() => {
    if (isFirstLoad && state.users.length === 0 && !state.loading) {
      fetchUsers();
    }
  }, [isFirstLoad, state.users.length, state.loading, fetchUsers]);

  return {
    // State
    users: state.users,
    loading: state.loading,
    error: state.error,
    pagination: state.pagination,
    searchParams: state.searchParams,

    // Actions
    fetchUsers,
    createUser,
    updateUser,
    deleteUser,
    searchUsers,
    refreshUsers,
    setPage,
    setLimit,
    setSortBy,
    clearError,
    reset,
    bulkDeleteUsers,
    checkUserExists,

    // Computed values
    isFirstLoad,
    hasUsers,
    totalUsers,
    currentPage,
    totalPages,
    hasNextPage,
    hasPreviousPage,
    isSearching,
  };
}

export default useUsers;