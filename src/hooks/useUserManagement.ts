'use client';

import { useState, useCallback } from 'react';
import { userService } from '@/services/userService';
import { 
  UserSecurityDTO, 
  SignonRequestDTO, 
  SignonResponseDTO,
  CreateUserSecurityRequest,
  UpdateUserSecurityRequest,
  ProgramContext,
  ScreenState
} from '@/types/user';

export function useUserManagement() {
  // Screen state management (equivalent to COBOL working storage)
  const [screenState, setScreenState] = useState<ScreenState>({
    currentPage: 0,
    message: '',
    messageType: 'info',
    loading: false,
    cursorField: undefined
  });

  // Program context (equivalent to COBOL COMMAREA)
  const [programContext, setProgramContext] = useState<ProgramContext>({
    fromProgram: '',
    fromTransaction: '',
    toProgram: '',
    programReenter: false,
    selectedUserId: undefined,
    pageNumber: 0,
    firstUserId: undefined,
    lastUserId: undefined,
    hasNextPage: false
  });

  // Current user data
  const [currentUser, setCurrentUser] = useState<UserSecurityDTO | null>(null);
  const [users, setUsers] = useState<UserSecurityDTO[]>([]);

  // Update screen state
  const updateScreenState = useCallback((updates: Partial<ScreenState>) => {
    setScreenState(prev => ({ ...prev, ...updates }));
  }, []);

  // Update program context
  const updateProgramContext = useCallback((updates: Partial<ProgramContext>) => {
    setProgramContext(prev => ({ ...prev, ...updates }));
  }, []);

  // Clear message after timeout (COBOL screen behavior)
  const clearMessage = useCallback(() => {
    setTimeout(() => {
      updateScreenState({ message: '', messageType: 'info' });
    }, 5000);
  }, [updateScreenState]);

  // Set message with type and optional auto-clear
  const setMessage = useCallback((message: string, type: 'success' | 'error' | 'info' = 'info', autoClear = true) => {
    updateScreenState({ message, messageType: type });
    if (autoClear && type !== 'error') {
      clearMessage();
    }
  }, [updateScreenState, clearMessage]);

  // Signon functionality (COSGN00C business logic)
  const signon = useCallback(async (credentials: SignonRequestDTO): Promise<SignonResponseDTO> => {
    updateScreenState({ loading: true, message: '' });

    try {
      // Validate inputs using service validation methods
      const userIdValidation = userService.validateUserId(credentials.userId);
      if (!userIdValidation.valid) {
        setMessage(userIdValidation.message, 'error');
        updateScreenState({ cursorField: 'userId' });
        throw new Error(userIdValidation.message);
      }

      const passwordValidation = userService.validatePassword(credentials.password);
      if (!passwordValidation.valid) {
        setMessage(passwordValidation.message, 'error');
        updateScreenState({ cursorField: 'password' });
        throw new Error(passwordValidation.message);
      }

      // Attempt signon
      const response = await userService.signon(credentials);
      
      if (response.success) {
        setMessage(response.message, 'success');
        updateProgramContext({
          fromProgram: 'COSGN00C',
          fromTransaction: 'CC00',
          toProgram: response.redirectProgram
        });
      } else {
        setMessage(response.message, 'error');
        // Position cursor based on error type
        if (response.message.includes('User ID') || response.message.includes('User not found')) {
          updateScreenState({ cursorField: 'userId' });
        } else {
          updateScreenState({ cursorField: 'password' });
        }
      }

      return response;
    } catch (error) {
      const errorMessage = error instanceof Error ? error.message : 'Authentication failed';
      setMessage(errorMessage, 'error');
      throw error;
    } finally {
      updateScreenState({ loading: false });
    }
  }, [setMessage, updateScreenState, updateProgramContext]);

  // Load users with pagination (COUSR00C business logic)
  const loadUsers = useCallback(async (page: number = 0, startFromUserId?: string) => {
    updateScreenState({ loading: true, message: '' });

    try {
      const response = await userService.getAllUsers({
        page,
        size: 10, // Display 10 users per page (COBOL business logic)
        sort: 'userId'
      });

      setUsers(response.users);
      updateProgramContext({
        pageNumber: response.pagination.page,
        hasNextPage: response.pagination.page < response.pagination.totalPages - 1,
        firstUserId: response.users.length > 0 ? response.users[0].userId : undefined,
        lastUserId: response.users.length > 0 ? response.users[response.users.length - 1].userId : undefined
      });

      if (response.users.length === 0) {
        setMessage('No users found', 'info');
      }

      return response;
    } catch (error) {
      const errorMessage = error instanceof Error ? error.message : 'Unable to lookup User...';
      setMessage(errorMessage, 'error');
      throw error;
    } finally {
      updateScreenState({ loading: false });
    }
  }, [setMessage, updateScreenState, updateProgramContext]);

  // Get user by ID (COUSR02C/COUSR03C business logic)
  const getUserById = useCallback(async (userId: string): Promise<UserSecurityDTO> => {
    updateScreenState({ loading: true, message: '' });

    try {
      // Validate User ID
      const userIdValidation = userService.validateUserId(userId);
      if (!userIdValidation.valid) {
        setMessage(userIdValidation.message, 'error');
        updateScreenState({ cursorField: 'userId' });
        throw new Error(userIdValidation.message);
      }

      const user = await userService.getUserById(userId);
      setCurrentUser(user);
      setMessage('Press PF5 key to save your updates ...', 'info');
      updateScreenState({ cursorField: 'firstName' });

      return user;
    } catch (error) {
      const errorMessage = error instanceof Error ? error.message : 'Unable to lookup User...';
      
      if (errorMessage.includes('not found')) {
        setMessage('User ID NOT found...', 'error');
        updateScreenState({ cursorField: 'userId' });
      } else {
        setMessage(errorMessage, 'error');
        updateScreenState({ cursorField: 'firstName' });
      }
      
      setCurrentUser(null);
      throw error;
    } finally {
      updateScreenState({ loading: false });
    }
  }, [setMessage, updateScreenState]);

  // Create user (COUSR01C business logic)
  const createUser = useCallback(async (userData: CreateUserSecurityRequest): Promise<UserSecurityDTO> => {
    updateScreenState({ loading: true, message: '' });

    try {
      const newUser = await userService.createUser(userData);
      setMessage(`User ${newUser.userId} has been added ...`, 'success');
      updateScreenState({ cursorField: 'firstName' });

      return newUser;
    } catch (error) {
      const errorMessage = error instanceof Error ? error.message : 'Unable to Add User...';
      
      if (errorMessage.includes('already exists')) {
        setMessage('User ID already exist...', 'error');
        updateScreenState({ cursorField: 'userId' });
      } else {
        setMessage(errorMessage, 'error');
        updateScreenState({ cursorField: 'firstName' });
      }
      
      throw error;
    } finally {
      updateScreenState({ loading: false });
    }
  }, [setMessage, updateScreenState]);

  // Update user (COUSR02C business logic)
  const updateUser = useCallback(async (userId: string, userData: UpdateUserSecurityRequest): Promise<UserSecurityDTO> => {
    updateScreenState({ loading: true, message: '' });

    try {
      const updatedUser = await userService.updateUser(userId, userData);
      setCurrentUser(updatedUser);
      setMessage(`User ${updatedUser.userId} has been updated ...`, 'success');

      return updatedUser;
    } catch (error) {
      const errorMessage = error instanceof Error ? error.message : 'Unable to Update User...';
      
      if (errorMessage.includes('not found')) {
        setMessage('User ID NOT found...', 'error');
        updateScreenState({ cursorField: 'userId' });
      } else {
        setMessage(errorMessage, 'error');
        updateScreenState({ cursorField: 'firstName' });
      }
      
      throw error;
    } finally {
      updateScreenState({ loading: false });
    }
  }, [setMessage, updateScreenState]);

  // Delete user (COUSR03C business logic)
  const deleteUser = useCallback(async (userId: string): Promise<void> => {
    updateScreenState({ loading: true, message: '' });

    try {
      await userService.deleteUser(userId);
      setCurrentUser(null);
      setMessage(`User ${userId} has been deleted ...`, 'success');
      updateScreenState({ cursorField: 'userId' });
    } catch (error) {
      const errorMessage = error instanceof Error ? error.message : 'Unable to Update User...';
      
      if (errorMessage.includes('not found')) {
        setMessage('User ID NOT found...', 'error');
        updateScreenState({ cursorField: 'userId' });
      } else {
        setMessage(errorMessage, 'error');
        updateScreenState({ cursorField: 'firstName' });
      }
      
      throw error;
    } finally {
      updateScreenState({ loading: false });
    }
  }, [setMessage, updateScreenState]);

  // Clear screen (PF4 key functionality)
  const clearScreen = useCallback(() => {
    setCurrentUser(null);
    updateScreenState({
      message: '',
      messageType: 'info',
      cursorField: 'userId'
    });
    updateProgramContext({
      selectedUserId: undefined
    });
  }, [updateScreenState, updateProgramContext]);

  // Exit functionality (PF3 key)
  const exit = useCallback(async (): Promise<{ success: boolean; message: string }> => {
    try {
      const response = await userService.exit();
      setMessage(response.message, 'info');
      return response;
    } catch (error) {
      setMessage('Exit failed', 'error');
      throw error;
    }
  }, [setMessage]);

  return {
    // State
    screenState,
    programContext,
    currentUser,
    users,
    
    // Actions
    signon,
    loadUsers,
    getUserById,
    createUser,
    updateUser,
    deleteUser,
    clearScreen,
    exit,
    
    // Utilities
    setMessage,
    updateScreenState,
    updateProgramContext,
    
    // Service utilities
    validateUserId: userService.validateUserId,
    validatePassword: userService.validatePassword,
    validateUserType: userService.validateUserType,
    validateFirstName: userService.validateFirstName,
    validateLastName: userService.validateLastName,
    formatUserId: userService.formatUserId,
    getRedirectProgram: userService.getRedirectProgram
  };
}