// src/hooks/useUserSecurity.ts

import { useState, useEffect, useCallback } from 'react';
import {
  UserSecurityDTO,
  CreateUserSecurityRequest,
  UpdateUserSecurityRequest,
  ChangePasswordRequest,
  UserListParams,
  UserSecurityFilters,
  UserSecurityState,
  PaginatedResponse,
  BulkUserOperation,
  BulkOperationResult,
  UserSecuritySummary,
  SignonRequestDTO,
  SignonResponseDTO
} from '@/types/userSecurity';
import { userSecurityService } from '@/services/userSecurityService';

interface UseUserSecurityReturn {
  // State
  state: UserSecurityState;
  
  // User Management
  fetchUsers: (params?: UserListParams) => Promise<void>;
  fetchUserById: (userId: string) => Promise<UserSecurityDTO | null>;
  createUser: (userData: CreateUserSecurityRequest) => Promise<UserSecurityDTO | null>;
  updateUser: (userId: string, userData: UpdateUserSecurityRequest) => Promise<UserSecurityDTO | null>;
  deleteUser: (userId: string) => Promise<boolean>;
  
  // User Status Management
  activateUser: (userId: string) => Promise<UserSecurityDTO | null>;
  deactivateUser: (userId: string) => Promise<UserSecurityDTO | null>;
  changePassword: (userId: string, passwordData: ChangePasswordRequest) => Promise<boolean>;
  
  // Authentication
  signon: (credentials: SignonRequestDTO) => Promise<SignonResponseDTO | null>;
  
  // Bulk Operations
  bulkOperation: (operation: BulkUserOperation) => Promise<BulkOperationResult | null>;
  
  // Search and Filter
  searchUsers: (query: string, filters?: UserSecurityFilters) => Promise<void>;
  setFilters: (filters: UserSecurityFilters) => void;
  clearFilters: () => void;
  
  // Pagination
  setPage: (page: number) => void;
  setPageSize: (size: number) => void;
  
  // Selection
  selectUser: (userId: string) => void;
  selectAllUsers: () => void;
  clearSelection: () => void;
  
  // Summary
  fetchSummary: () => Promise<UserSecuritySummary | null>;
  
  // Utility
  clearError: () => void;
  refreshUsers: () => Promise<void>;
  resetState: () => void;
}

const initialState: UserSecurityState = {
  users: [],
  currentUser: null,
  loading: false,
  error: null,
  filters: {},
  pagination: {
    page: 0,
    limit: 20,
    total: 0,
    totalPages: 0,
  },
  selectedUsers: [],
  summary: null,
};

export const useUserSecurity = (): UseUserSecurityReturn => {
  const [state, setState] = useState<UserSecurityState>(initialState);

  // Helper function to update state
  const updateState = useCallback((updates: Partial<UserSecurityState>) => {
    setState(prev => ({ ...prev, ...updates }));
  }, []);

  // Helper function to handle errors
  const handleError = useCallback((error: unknown, operation: string) => {
    const errorMessage = error instanceof Error ? error.message : `${operation} failed`;
    updateState({ error: errorMessage, loading: false });
    console.error(`${operation} error:`, error);
  }, [updateState]);

  // Fetch users with pagination and filters
  const fetchUsers = useCallback(async (params?: UserListParams) => {
    try {
      updateState({ loading: true, error: null });
      
      const requestParams = {
        page: params?.page ?? state.pagination.page,
        limit: params?.limit ?? state.pagination.limit,
        ...state.filters,
        ...params,
      };

      const response: PaginatedResponse<UserSecurityDTO> = await userSecurityService.getUsers(requestParams);
      
      updateState({
        users: response.data,
        pagination: response.pagination,
        loading: false,
      });
    } catch (error) {
      handleError(error, 'Fetch users');
    }
  }, [state.pagination.page, state.pagination.limit, state.filters, updateState, handleError]);

  // Fetch single user by ID
  const fetchUserById = useCallback(async (userId: string): Promise<UserSecurityDTO | null> => {
    try {
      updateState({ loading: true, error: null });
      const user = await userSecurityService.getUserById(userId);
      updateState({ currentUser: user, loading: false });
      return user;
    } catch (error) {
      handleError(error, 'Fetch user');
      return null;
    }
  }, [updateState, handleError]);

  // Create new user
  const createUser = useCallback(async (userData: CreateUserSecurityRequest): Promise<UserSecurityDTO | null> => {
    try {
      updateState({ loading: true, error: null });
      const newUser = await userSecurityService.createUser(userData);
      
      // Add new user to the list
      updateState({
        users: [newUser, ...state.users],
        loading: false,
      });
      
      return newUser;
    } catch (error) {
      handleError(error, 'Create user');
      return null;
    }
  }, [state.users, updateState, handleError]);

  // Update existing user
  const updateUser = useCallback(async (userId: string, userData: UpdateUserSecurityRequest): Promise<UserSecurityDTO | null> => {
    try {
      updateState({ loading: true, error: null });
      const updatedUser = await userSecurityService.updateUser(userId, userData);
      
      // Update user in the list
      const updatedUsers = state.users.map(user => 
        user.userId === userId ? updatedUser : user
      );
      
      updateState({
        users: updatedUsers,
        currentUser: state.currentUser?.userId === userId ? updatedUser : state.currentUser,
        loading: false,
      });
      
      return updatedUser;
    } catch (error) {
      handleError(error, 'Update user');
      return null;
    }
  }, [state.users, state.currentUser, updateState, handleError]);

  // Delete user
  const deleteUser = useCallback(async (userId: string): Promise<boolean> => {
    try {
      updateState({ loading: true, error: null });
      await userSecurityService.deleteUser(userId);
      
      // Remove user from the list
      const filteredUsers = state.users.filter(user => user.userId !== userId);
      const filteredSelection = state.selectedUsers.filter(id => id !== userId);
      
      updateState({
        users: filteredUsers,
        selectedUsers: filteredSelection,
        currentUser: state.currentUser?.userId === userId ? null : state.currentUser,
        loading: false,
      });
      
      return true;
    } catch (error) {
      handleError(error, 'Delete user');
      return false;
    }
  }, [state.users, state.selectedUsers, state.currentUser, updateState, handleError]);

  // Activate user
  const activateUser = useCallback(async (userId: string): Promise<UserSecurityDTO | null> => {
    try {
      updateState({ loading: true, error: null });
      const activatedUser = await userSecurityService.activateUser(userId);
      
      // Update user in the list
      const updatedUsers = state.users.map(user => 
        user.userId === userId ? activatedUser : user
      );
      
      updateState({
        users: updatedUsers,
        currentUser: state.currentUser?.userId === userId ? activatedUser : state.currentUser,
        loading: false,
      });
      
      return activatedUser;
    } catch (error) {
      handleError(error, 'Activate user');
      return null;
    }
  }, [state.users, state.currentUser, updateState, handleError]);

  // Deactivate user
  const deactivateUser = useCallback(async (userId: string): Promise<UserSecurityDTO | null> => {
    try {
      updateState({ loading: true, error: null });
      const deactivatedUser = await userSecurityService.deactivateUser(userId);
      
      // Update user in the list
      const updatedUsers = state.users.map(user => 
        user.userId === userId ? deactivatedUser : user
      );
      
      updateState({
        users: updatedUsers,
        currentUser: state.currentUser?.userId === userId ? deactivatedUser : state.currentUser,
        loading: false,
      });
      
      return deactivatedUser;
    } catch (error) {
      handleError(error, 'Deactivate user');
      return null;
    }
  }, [state.users, state.currentUser, updateState, handleError]);

  // Change password
  const changePassword = useCallback(async (userId: string, passwordData: ChangePasswordRequest): Promise<boolean> => {
    try {
      updateState({ loading: true, error: null });
      await userSecurityService.changePassword(userId, passwordData);
      updateState({ loading: false });
      return true;
    } catch (error) {
      handleError(error, 'Change password');
      return false;
    }
  }, [updateState, handleError]);

  // Sign on
  const signon = useCallback(async (credentials: SignonRequestDTO): Promise<SignonResponseDTO | null> => {
    try {
      updateState({ loading: true, error: null });
      const response = await userSecurityService.signon(credentials);
      updateState({ loading: false });
      return response;
    } catch (error) {
      handleError(error, 'Sign on');
      return null;
    }
  }, [updateState, handleError]);

  // Bulk operations
  const bulkOperation = useCallback(async (operation: BulkUserOperation): Promise<BulkOperationResult | null> => {
    try {
      updateState({ loading: true, error: null });
      const result = await userSecurityService.bulkUserOperation(operation);
      
      // Refresh users list after bulk operation
      await fetchUsers();
      
      return result;
    } catch (error) {
      handleError(error, 'Bulk operation');
      return null;
    }
  }, [updateState, handleError, fetchUsers]);

  // Search users
  const searchUsers = useCallback(async (query: string, filters?: UserSecurityFilters) => {
    try {
      updateState({ loading: true, error: null });
      
      const searchFilters = { ...state.filters, ...filters };
      const response = await userSecurityService.searchUsers(query, searchFilters);
      
      updateState({
        users: response.data,
        pagination: response.pagination,
        filters: searchFilters,
        loading: false,
      });
    } catch (error) {
      handleError(error, 'Search users');
    }
  }, [state.filters, updateState, handleError]);

  // Set filters
  const setFilters = useCallback((filters: UserSecurityFilters) => {
    updateState({ filters });
  }, [updateState]);

  // Clear filters
  const clearFilters = useCallback(() => {
    updateState({ filters: {} });
  }, [updateState]);

  // Set page
  const setPage = useCallback((page: number) => {
    updateState({
      pagination: { ...state.pagination, page }
    });
  }, [state.pagination, updateState]);

  // Set page size
  const setPageSize = useCallback((limit: number) => {
    updateState({
      pagination: { ...state.pagination, limit, page: 0 }
    });
  }, [state.pagination, updateState]);

  // Select user
  const selectUser = useCallback((userId: string) => {
    const isSelected = state.selectedUsers.includes(userId);
    const selectedUsers = isSelected
      ? state.selectedUsers.filter(id => id !== userId)
      : [...state.selectedUsers, userId];
    
    updateState({ selectedUsers });
  }, [state.selectedUsers, updateState]);

  // Select all users
  const selectAllUsers = useCallback(() => {
    const allUserIds = state.users.map(user => user.userId);
    updateState({ selectedUsers: allUserIds });
  }, [state.users, updateState]);

  // Clear selection
  const clearSelection = useCallback(() => {
    updateState({ selectedUsers: [] });
  }, [updateState]);

  // Fetch summary
  const fetchSummary = useCallback(async (): Promise<UserSecuritySummary | null> => {
    try {
      updateState({ loading: true, error: null });
      const summary = await userSecurityService.getUserSummary();
      updateState({ summary, loading: false });
      return summary;
    } catch (error) {
      handleError(error, 'Fetch summary');
      return null;
    }
  }, [updateState, handleError]);

  // Clear error
  const clearError = useCallback(() => {
    updateState({ error: null });
  }, [updateState]);

  // Refresh users
  const refreshUsers = useCallback(async () => {
    await fetchUsers();
  }, [fetchUsers]);

  // Reset state
  const resetState = useCallback(() => {
    setState(initialState);
  }, []);

  // Load initial data
  useEffect(() => {
    fetchUsers();
  }, []);

  return {
    state,
    fetchUsers,
    fetchUserById,
    createUser,
    updateUser,
    deleteUser,
    activateUser,
    deactivateUser,
    changePassword,
    signon,
    bulkOperation,
    searchUsers,
    setFilters,
    clearFilters,
    setPage,
    setPageSize,
    selectUser,
    selectAllUsers,
    clearSelection,
    fetchSummary,
    clearError,
    refreshUsers,
    resetState,
  };
};