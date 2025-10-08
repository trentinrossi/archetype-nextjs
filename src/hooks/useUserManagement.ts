'use client';

import { useState, useCallback, useRef, useEffect } from 'react';
import { userService } from '@/services/userService';
import {
  User,
  UserCreateRequest,
  UserUpdateRequest,
  UserListResponse,
  Pageable,
  UserMessage
} from '@/types/user';

export interface UseUserManagementOptions {
  initialPage?: number;
  initialSize?: number;
  autoFetch?: boolean;
}

export interface UseUserManagementReturn {
  // State
  users: User[];
  isLoading: boolean;
  isCreating: boolean;
  isUpdating: boolean;
  isDeleting: boolean;
  error: string | null;
  pagination: UserListResponse | null;
  message: UserMessage | null;
  
  // User operations
  fetchUsers: (params?: Pageable) => Promise<void>;
  createUser: (data: UserCreateRequest) => Promise<User | null>;
  updateUser: (userId: string, data: UserUpdateRequest) => Promise<User | null>;
  deleteUser: (userId: string) => Promise<boolean>;
  getUserById: (userId: string) => Promise<User | null>;
  
  // Pagination
  goToPage: (page: number) => Promise<void>;
  changePageSize: (size: number) => Promise<void>;
  goToNextPage: () => Promise<void>;
  goToPreviousPage: () => Promise<void>;
  
  // Search functionality
  searchUsersFromId: (startUserId: string, params?: Pageable) => Promise<void>;
  
  // Utility operations
  refreshUsers: () => Promise<void>;
  
  // State management
  clearError: () => void;
  clearMessage: () => void;
  setMessage: (message: UserMessage) => void;
  resetState: () => void;
  
  // Batch operations
  deleteMultipleUsers: (userIds: string[]) => Promise<string[]>;
  getUsersByIds: (userIds: string[]) => Promise<User[]>;
}

const DEFAULT_PAGE_SIZE = 10;
const DEFAULT_PAGE = 0;

export function useUserManagement(options: UseUserManagementOptions = {}): UseUserManagementReturn {
  const {
    initialPage = DEFAULT_PAGE,
    initialSize = DEFAULT_PAGE_SIZE,
    autoFetch = true
  } = options;

  // State
  const [users, setUsers] = useState<User[]>([]);
  const [isLoading, setIsLoading] = useState(false);
  const [isCreating, setIsCreating] = useState(false);
  const [isUpdating, setIsUpdating] = useState(false);
  const [isDeleting, setIsDeleting] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [pagination, setPagination] = useState<UserListResponse | null>(null);
  const [message, setMessageState] = useState<UserMessage | null>(null);
  const [currentParams, setCurrentParams] = useState<Pageable>({
    page: initialPage,
    size: initialSize
  });

  // Refs for tracking component mount and preventing race conditions
  const isMountedRef = useRef(true);
  const abortControllerRef = useRef<AbortController | null>(null);

  // Cleanup on unmount
  useEffect(() => {
    return () => {
      isMountedRef.current = false;
      if (abortControllerRef.current) {
        abortControllerRef.current.abort();
      }
    };
  }, []);

  // Auto-fetch on mount
  useEffect(() => {
    if (autoFetch) {
      fetchUsers(currentParams);
    }
  }, [autoFetch]);

  // Helper function to handle errors
  const handleError = useCallback((error: unknown, defaultMessage: string) => {
    const errorMessage = error instanceof Error ? error.message : defaultMessage;
    if (isMountedRef.current) {
      setError(errorMessage);
      setMessageState({
        type: 'error',
        message: errorMessage
      });
    }
    console.error(defaultMessage, error);
    return errorMessage;
  }, []);

  // Helper function to update state safely
  const safeSetState = useCallback(<T>(setter: (value: T) => void, value: T) => {
    if (isMountedRef.current) {
      setter(value);
    }
  }, []);

  // Fetch users
  const fetchUsers = useCallback(async (params?: Pageable): Promise<void> => {
    try {
      // Cancel previous request
      if (abortControllerRef.current) {
        abortControllerRef.current.abort();
      }
      abortControllerRef.current = new AbortController();

      safeSetState(setIsLoading, true);
      safeSetState(setError, null);

      const searchParams = params || currentParams;
      const response: UserListResponse = await userService.getUsers(searchParams);

      if (isMountedRef.current) {
        setUsers(response.users || []);
        setPagination(response);
        setCurrentParams(searchParams);
      }
    } catch (error) {
      if (error instanceof Error && error.name === 'AbortError') {
        return; // Request was cancelled, don't update state
      }
      handleError(error, 'Failed to fetch users');
    } finally {
      safeSetState(setIsLoading, false);
    }
  }, [currentParams, handleError, safeSetState]);

  // Create user
  const createUser = useCallback(async (data: UserCreateRequest): Promise<User | null> => {
    try {
      safeSetState(setIsCreating, true);
      safeSetState(setError, null);

      const newUser = await userService.createUser(data);
      
      if (isMountedRef.current) {
        setMessageState({
          type: 'success',
          message: `User ${newUser.userId} has been added...`
        });
        
        // Refresh to get updated list
        await fetchUsers(currentParams);
      }

      return newUser;
    } catch (error) {
      handleError(error, 'Failed to create user');
      return null;
    } finally {
      safeSetState(setIsCreating, false);
    }
  }, [currentParams, fetchUsers, handleError, safeSetState]);

  // Update user
  const updateUser = useCallback(async (userId: string, data: UserUpdateRequest): Promise<User | null> => {
    try {
      safeSetState(setIsUpdating, true);
      safeSetState(setError, null);

      const updatedUser = await userService.updateUser(userId, data);
      
      if (isMountedRef.current) {
        // Update user in current list
        setUsers(prev => prev.map(user => user.userId === userId ? updatedUser : user));
        setMessageState({
          type: 'success',
          message: `User ${updatedUser.userId} has been updated...`
        });
      }

      return updatedUser;
    } catch (error) {
      handleError(error, 'Failed to update user');
      return null;
    } finally {
      safeSetState(setIsUpdating, false);
    }
  }, [handleError, safeSetState]);

  // Delete user
  const deleteUser = useCallback(async (userId: string): Promise<boolean> => {
    try {
      safeSetState(setIsDeleting, true);
      safeSetState(setError, null);

      await userService.deleteUser(userId);
      
      if (isMountedRef.current) {
        // Remove user from current list
        setUsers(prev => prev.filter(user => user.userId !== userId));
        setMessageState({
          type: 'success',
          message: `User ${userId} has been deleted...`
        });
        
        // Refresh to get updated pagination
        await fetchUsers(currentParams);
      }

      return true;
    } catch (error) {
      handleError(error, 'Failed to delete user');
      return false;
    } finally {
      safeSetState(setIsDeleting, false);
    }
  }, [currentParams, fetchUsers, handleError, safeSetState]);

  // Get user by ID
  const getUserById = useCallback(async (userId: string): Promise<User | null> => {
    try {
      safeSetState(setError, null);
      const user = await userService.getUserById(userId);
      return user;
    } catch (error) {
      handleError(error, 'Failed to fetch user');
      return null;
    }
  }, [handleError, safeSetState]);

  // Search users from specific ID
  const searchUsersFromId = useCallback(async (startUserId: string, params?: Pageable): Promise<void> => {
    try {
      safeSetState(setIsLoading, true);
      safeSetState(setError, null);

      const searchParams = params || currentParams;
      const response: UserListResponse = await userService.searchUsersFromId(startUserId, searchParams);

      if (isMountedRef.current) {
        setUsers(response.users || []);
        setPagination(response);
        setCurrentParams(searchParams);
      }
    } catch (error) {
      handleError(error, 'Failed to search users');
    } finally {
      safeSetState(setIsLoading, false);
    }
  }, [currentParams, handleError, safeSetState]);

  // Pagination methods
  const goToPage = useCallback(async (page: number): Promise<void> => {
    const newParams = { ...currentParams, page };
    await fetchUsers(newParams);
  }, [currentParams, fetchUsers]);

  const changePageSize = useCallback(async (size: number): Promise<void> => {
    const newParams = { ...currentParams, size, page: 0 };
    await fetchUsers(newParams);
  }, [currentParams, fetchUsers]);

  const goToNextPage = useCallback(async (): Promise<void> => {
    if (pagination && !pagination.last) {
      await goToPage(pagination.page + 1);
    }
  }, [pagination, goToPage]);

  const goToPreviousPage = useCallback(async (): Promise<void> => {
    if (pagination && !pagination.first) {
      await goToPage(pagination.page - 1);
    }
  }, [pagination, goToPage]);

  // Utility methods
  const refreshUsers = useCallback(async (): Promise<void> => {
    await fetchUsers(currentParams);
  }, [currentParams, fetchUsers]);

  // State management
  const clearError = useCallback(() => {
    safeSetState(setError, null);
  }, [safeSetState]);

  const clearMessage = useCallback(() => {
    safeSetState(setMessageState, null);
  }, [safeSetState]);

  const setMessage = useCallback((newMessage: UserMessage) => {
    safeSetState(setMessageState, newMessage);
  }, [safeSetState]);

  const resetState = useCallback(() => {
    if (isMountedRef.current) {
      setUsers([]);
      setPagination(null);
      setError(null);
      setMessageState(null);
      setCurrentParams({
        page: initialPage,
        size: initialSize
      });
    }
  }, [initialPage, initialSize]);

  // Batch operations
  const deleteMultipleUsers = useCallback(async (userIds: string[]): Promise<string[]> => {
    const failedIds: string[] = [];
    
    try {
      safeSetState(setIsDeleting, true);
      safeSetState(setError, null);

      const deletePromises = userIds.map(async (userId) => {
        try {
          await userService.deleteUser(userId);
          return { userId, success: true };
        } catch (error) {
          console.error(`Failed to delete user ${userId}:`, error);
          return { userId, success: false };
        }
      });

      const results = await Promise.all(deletePromises);
      
      results.forEach(result => {
        if (!result.success) {
          failedIds.push(result.userId);
        }
      });

      if (isMountedRef.current) {
        const successfulIds = results.filter(r => r.success).map(r => r.userId);
        setUsers(prev => prev.filter(user => !successfulIds.includes(user.userId)));
        
        if (successfulIds.length > 0) {
          setMessageState({
            type: 'success',
            message: `${successfulIds.length} users deleted successfully`
          });
        }
        
        await fetchUsers(currentParams);
      }

      if (failedIds.length > 0) {
        handleError(new Error(`Failed to delete ${failedIds.length} users`), 'Some users could not be deleted');
      }
    } catch (error) {
      handleError(error, 'Failed to delete users');
      return userIds; // All failed
    } finally {
      safeSetState(setIsDeleting, false);
    }

    return failedIds;
  }, [currentParams, fetchUsers, handleError, safeSetState]);

  const getUsersByIds = useCallback(async (userIds: string[]): Promise<User[]> => {
    try {
      safeSetState(setError, null);
      const users = await userService.getUsersByIds(userIds);
      return users;
    } catch (error) {
      handleError(error, 'Failed to fetch users by IDs');
      return [];
    }
  }, [handleError, safeSetState]);

  return {
    // State
    users,
    isLoading,
    isCreating,
    isUpdating,
    isDeleting,
    error,
    pagination,
    message,
    
    // User operations
    fetchUsers,
    createUser,
    updateUser,
    deleteUser,
    getUserById,
    
    // Pagination
    goToPage,
    changePageSize,
    goToNextPage,
    goToPreviousPage,
    
    // Search functionality
    searchUsersFromId,
    
    // Utility operations
    refreshUsers,
    
    // State management
    clearError,
    clearMessage,
    setMessage,
    resetState,
    
    // Batch operations
    deleteMultipleUsers,
    getUsersByIds
  };
}

export default useUserManagement;