'use client';

import { useState, useCallback, useRef } from 'react';
import userSecurityService from '@/services/userSecurityService';
import {
  UserSecurity,
  CreateUserSecurityRequest,
  UpdateUserSecurityRequest,
  ChangePasswordRequest,
  GetUsersRequest,
  GetUsersResponse,
  UseUserSecurityReturn
} from '@/types/userSecurity';

export function useUserSecurity(): UseUserSecurityReturn {
  const [users, setUsers] = useState<UserSecurity[]>([]);
  const [currentUser, setCurrentUser] = useState<UserSecurity | null>(null);
  const [loading, setLoading] = useState<boolean>(false);
  const [error, setError] = useState<string | null>(null);
  const [pagination, setPagination] = useState<GetUsersResponse['pagination'] | null>(null);
  
  const abortControllerRef = useRef<AbortController | null>(null);

  const clearError = useCallback(() => {
    setError(null);
  }, []);

  const handleError = useCallback((errorMessage: string) => {
    setError(errorMessage);
    setLoading(false);
  }, []);

  const fetchUsers = useCallback(async (params: GetUsersRequest = {}) => {
    try {
      // Cancel previous request if still pending
      if (abortControllerRef.current) {
        abortControllerRef.current.abort();
      }

      abortControllerRef.current = new AbortController();
      
      setLoading(true);
      setError(null);

      const response = await userSecurityService.getUsers(params);

      if (!response.success) {
        handleError(response.error || 'Failed to fetch users');
        return;
      }

      if (response.data) {
        setUsers(response.data.users);
        setPagination(response.data.pagination);
      }
    } catch (err) {
      if (err instanceof Error && err.name !== 'AbortError') {
        handleError(err.message);
      }
    } finally {
      setLoading(false);
      abortControllerRef.current = null;
    }
  }, [handleError]);

  const fetchUserById = useCallback(async (userId: string): Promise<UserSecurity | null> => {
    try {
      setLoading(true);
      setError(null);

      const response = await userSecurityService.getUserById(userId);

      if (!response.success) {
        handleError(response.error || 'Failed to fetch user');
        return null;
      }

      if (response.data?.user) {
        setCurrentUser(response.data.user);
        return response.data.user;
      }

      return null;
    } catch (err) {
      handleError(err instanceof Error ? err.message : 'Failed to fetch user');
      return null;
    } finally {
      setLoading(false);
    }
  }, [handleError]);

  const createUser = useCallback(async (userData: CreateUserSecurityRequest): Promise<boolean> => {
    try {
      setLoading(true);
      setError(null);

      // Validate required fields
      if (!userData.username || !userData.email || !userData.password) {
        handleError('Username, email, and password are required');
        return false;
      }

      if (userData.password !== userData.confirmPassword) {
        handleError('Passwords do not match');
        return false;
      }

      const response = await userSecurityService.createUser(userData);

      if (!response.success) {
        handleError(response.error || 'Failed to create user');
        return false;
      }

      // Refresh users list after successful creation
      await fetchUsers();
      return true;
    } catch (err) {
      handleError(err instanceof Error ? err.message : 'Failed to create user');
      return false;
    } finally {
      setLoading(false);
    }
  }, [handleError, fetchUsers]);

  const updateUser = useCallback(async (
    userId: string, 
    userData: UpdateUserSecurityRequest
  ): Promise<boolean> => {
    try {
      setLoading(true);
      setError(null);

      if (!userId) {
        handleError('User ID is required');
        return false;
      }

      const response = await userSecurityService.updateUser(userId, userData);

      if (!response.success) {
        handleError(response.error || 'Failed to update user');
        return false;
      }

      // Update the user in the current list if it exists
      if (response.data) {
        setUsers(prevUsers => 
          prevUsers.map(user => 
            user.userId === userId ? response.data! : user
          )
        );

        // Update current user if it's the same user
        if (currentUser?.userId === userId) {
          setCurrentUser(response.data);
        }
      }

      return true;
    } catch (err) {
      handleError(err instanceof Error ? err.message : 'Failed to update user');
      return false;
    } finally {
      setLoading(false);
    }
  }, [handleError, currentUser]);

  const deleteUser = useCallback(async (userId: string): Promise<boolean> => {
    try {
      setLoading(true);
      setError(null);

      if (!userId) {
        handleError('User ID is required');
        return false;
      }

      const response = await userSecurityService.deleteUser(userId);

      if (!response.success) {
        handleError(response.error || 'Failed to delete user');
        return false;
      }

      // Remove the user from the current list
      setUsers(prevUsers => prevUsers.filter(user => user.userId !== userId));

      // Clear current user if it's the deleted user
      if (currentUser?.userId === userId) {
        setCurrentUser(null);
      }

      // Update pagination if needed
      if (pagination) {
        setPagination(prev => prev ? {
          ...prev,
          total: prev.total - 1,
          totalPages: Math.ceil((prev.total - 1) / prev.limit)
        } : null);
      }

      return true;
    } catch (err) {
      handleError(err instanceof Error ? err.message : 'Failed to delete user');
      return false;
    } finally {
      setLoading(false);
    }
  }, [handleError, currentUser, pagination]);

  const activateUser = useCallback(async (userId: string, reason?: string): Promise<boolean> => {
    try {
      setLoading(true);
      setError(null);

      if (!userId) {
        handleError('User ID is required');
        return false;
      }

      const response = await userSecurityService.activateUser(userId, { reason });

      if (!response.success) {
        handleError(response.error || 'Failed to activate user');
        return false;
      }

      // Update the user's active status in the current list
      if (response.data?.user) {
        setUsers(prevUsers => 
          prevUsers.map(user => 
            user.userId === userId ? response.data!.user! : user
          )
        );

        // Update current user if it's the same user
        if (currentUser?.userId === userId) {
          setCurrentUser(response.data.user);
        }
      } else {
        // Fallback: update isActive status directly
        setUsers(prevUsers => 
          prevUsers.map(user => 
            user.userId === userId ? { ...user, isActive: true } : user
          )
        );

        if (currentUser?.userId === userId) {
          setCurrentUser(prev => prev ? { ...prev, isActive: true } : null);
        }
      }

      return true;
    } catch (err) {
      handleError(err instanceof Error ? err.message : 'Failed to activate user');
      return false;
    } finally {
      setLoading(false);
    }
  }, [handleError, currentUser]);

  const deactivateUser = useCallback(async (userId: string, reason?: string): Promise<boolean> => {
    try {
      setLoading(true);
      setError(null);

      if (!userId) {
        handleError('User ID is required');
        return false;
      }

      const response = await userSecurityService.deactivateUser(userId, { reason });

      if (!response.success) {
        handleError(response.error || 'Failed to deactivate user');
        return false;
      }

      // Update the user's active status in the current list
      if (response.data?.user) {
        setUsers(prevUsers => 
          prevUsers.map(user => 
            user.userId === userId ? response.data!.user! : user
          )
        );

        // Update current user if it's the same user
        if (currentUser?.userId === userId) {
          setCurrentUser(response.data.user);
        }
      } else {
        // Fallback: update isActive status directly
        setUsers(prevUsers => 
          prevUsers.map(user => 
            user.userId === userId ? { ...user, isActive: false } : user
          )
        );

        if (currentUser?.userId === userId) {
          setCurrentUser(prev => prev ? { ...prev, isActive: false } : null);
        }
      }

      return true;
    } catch (err) {
      handleError(err instanceof Error ? err.message : 'Failed to deactivate user');
      return false;
    } finally {
      setLoading(false);
    }
  }, [handleError, currentUser]);

  const changeUserPassword = useCallback(async (
    userId: string, 
    passwordData: ChangePasswordRequest
  ): Promise<boolean> => {
    try {
      setLoading(true);
      setError(null);

      if (!userId) {
        handleError('User ID is required');
        return false;
      }

      // Validate password data
      if (!passwordData.currentPassword || !passwordData.newPassword) {
        handleError('Current password and new password are required');
        return false;
      }

      if (passwordData.newPassword !== passwordData.confirmNewPassword) {
        handleError('New passwords do not match');
        return false;
      }

      if (passwordData.currentPassword === passwordData.newPassword) {
        handleError('New password must be different from current password');
        return false;
      }

      const response = await userSecurityService.changePassword(userId, passwordData);

      if (!response.success) {
        handleError(response.error || 'Failed to change password');
        return false;
      }

      // Update user's mustChangePassword flag if needed
      if (response.data?.requiresReauth === false) {
        setUsers(prevUsers => 
          prevUsers.map(user => 
            user.userId === userId ? { ...user, mustChangePassword: false } : user
          )
        );

        if (currentUser?.userId === userId) {
          setCurrentUser(prev => prev ? { ...prev, mustChangePassword: false } : null);
        }
      }

      return true;
    } catch (err) {
      handleError(err instanceof Error ? err.message : 'Failed to change password');
      return false;
    } finally {
      setLoading(false);
    }
  }, [handleError, currentUser]);

  // Cleanup function to abort pending requests
  const cleanup = useCallback(() => {
    if (abortControllerRef.current) {
      abortControllerRef.current.abort();
      abortControllerRef.current = null;
    }
  }, []);

  return {
    users,
    currentUser,
    loading,
    error,
    pagination,
    fetchUsers,
    fetchUserById,
    createUser,
    updateUser,
    deleteUser,
    activateUser,
    deactivateUser,
    changeUserPassword,
    clearError,
  };
}

export default useUserSecurity;