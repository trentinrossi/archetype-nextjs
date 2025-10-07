'use client';

import React, { useState, useEffect, useCallback } from 'react';
import { useRouter } from 'next/navigation';
import { Button } from '@/components/ui/Button';
import { 
  CreateUserSecurityRequest,
  UserFormData,
  UserFormErrors,
  USER_VALIDATION_RULES,
  USER_TYPE_LABELS,
  UserType
} from '@/types/user';
import { userService } from '@/services/userService';

interface AddUserProps {
  onUserCreated?: (userId: string) => void;
  onExit?: () => void;
  className?: string;
}

interface AddUserState {
  formData: UserFormData;
  errors: UserFormErrors;
  isSubmitting: boolean;
  message: string;
  messageType: 'success' | 'error' | '';
  isFormDirty: boolean;
}

const AddUser: React.FC<AddUserProps> = ({
  onUserCreated,
  onExit,
  className = '',
}) => {
  const router = useRouter();
  const [state, setState] = useState<AddUserState>({
    formData: {
      userId: '',
      firstName: '',
      lastName: '',
      password: '',
      userType: 'G',
    },
    errors: {},
    isSubmitting: false,
    message: '',
    messageType: '',
    isFormDirty: false,
  });

  const updateState = useCallback((updates: Partial<AddUserState>) => {
    setState(prev => ({ ...prev, ...updates }));
  }, []);

  const validateField = useCallback((field: keyof UserFormData, value: string): string => {
    switch (field) {
      case 'userId':
        if (!value.trim()) {
          return 'User ID can NOT be empty...';
        }
        if (value.length > 8) {
          return 'User ID must be 8 characters or less';
        }
        return '';

      case 'firstName':
        if (!value.trim()) {
          return 'First Name can NOT be empty...';
        }
        if (value.length > 20) {
          return 'First Name must be 20 characters or less';
        }
        return '';

      case 'lastName':
        if (!value.trim()) {
          return 'Last Name can NOT be empty...';
        }
        if (value.length > 20) {
          return 'Last Name must be 20 characters or less';
        }
        return '';

      case 'password':
        if (!value.trim()) {
          return 'Password can NOT be empty...';
        }
        if (value.length > 8) {
          return 'Password must be 8 characters or less';
        }
        return '';

      case 'userType':
        if (!value || !['A', 'G'].includes(value)) {
          return 'User Type can NOT be empty...';
        }
        return '';

      default:
        return '';
    }
  }, []);

  const validateForm = useCallback((): UserFormErrors => {
    const errors: UserFormErrors = {};

    Object.keys(state.formData).forEach(key => {
      const field = key as keyof UserFormData;
      const value = state.formData[field];
      const error = validateField(field, value);
      if (error) {
        errors[field] = error;
      }
    });

    return errors;
  }, [state.formData, validateField]);

  const handleInputChange = useCallback((field: keyof UserFormData, value: string) => {
    let processedValue = value;

    // Apply field-specific processing
    switch (field) {
      case 'userId':
        processedValue = value.toUpperCase().slice(0, 8);
        break;
      case 'firstName':
      case 'lastName':
        processedValue = value.slice(0, 20);
        break;
      case 'password':
        processedValue = value.slice(0, 8);
        break;
    }

    const newFormData = { ...state.formData, [field]: processedValue };
    const fieldError = validateField(field, processedValue);
    const newErrors = { ...state.errors };

    if (fieldError) {
      newErrors[field] = fieldError;
    } else {
      delete newErrors[field];
    }

    updateState({
      formData: newFormData,
      errors: newErrors,
      isFormDirty: true,
      message: '',
      messageType: '',
    });
  }, [state.formData, state.errors, validateField, updateState]);

  const handleSubmit = useCallback(async () => {
    if (state.isSubmitting) return;

    const formErrors = validateForm();
    
    if (Object.keys(formErrors).length > 0) {
      const firstError = Object.values(formErrors)[0];
      updateState({
        errors: formErrors,
        message: firstError,
        messageType: 'error',
      });
      return;
    }

    updateState({ isSubmitting: true, errors: {}, message: '', messageType: '' });

    try {
      const createUserRequest: CreateUserSecurityRequest = {
        userId: state.formData.userId,
        firstName: state.formData.firstName,
        lastName: state.formData.lastName,
        password: state.formData.password,
        userType: state.formData.userType,
      };

      await userService.createUser(createUserRequest);

      // Clear form and show success message
      updateState({
        isSubmitting: false,
        formData: {
          userId: '',
          firstName: '',
          lastName: '',
          password: '',
          userType: 'G',
        },
        message: `User ${createUserRequest.userId} has been added...`,
        messageType: 'success',
        isFormDirty: false,
      });

      if (onUserCreated) {
        onUserCreated(createUserRequest.userId);
      }
    } catch (error: any) {
      let errorMessage = 'Unable to Add User...';
      
      if (error.message) {
        if (error.message.includes('already exist')) {
          errorMessage = 'User ID already exist...';
        } else {
          errorMessage = error.message;
        }
      }

      updateState({
        isSubmitting: false,
        message: errorMessage,
        messageType: 'error',
      });
    }
  }, [state.formData, state.isSubmitting, validateForm, onUserCreated, updateState]);

  const handleClear = useCallback(() => {
    updateState({
      formData: {
        userId: '',
        firstName: '',
        lastName: '',
        password: '',
        userType: 'G',
      },
      errors: {},
      message: '',
      messageType: '',
      isFormDirty: false,
    });

    // Focus on first field
    setTimeout(() => {
      const firstNameInput = document.getElementById('firstName');
      if (firstNameInput) {
        firstNameInput.focus();
      }
    }, 0);
  }, [updateState]);

  const handleExit = useCallback(async () => {
    if (state.isSubmitting) return;

    if (onExit) {
      onExit();
    } else {
      router.push('/users');
    }
  }, [state.isSubmitting, onExit, router]);

  const handleKeyDown = useCallback(async (event: KeyboardEvent) => {
    if (state.isSubmitting) return;

    switch (event.key) {
      case 'Enter':
        event.preventDefault();
        await handleSubmit();
        break;
      case 'F3':
        event.preventDefault();
        await handleExit();
        break;
      case 'F4':
        event.preventDefault();
        handleClear();
        break;
      default:
        if (event.key.startsWith('F') && !['F3', 'F4'].includes(event.key)) {
          event.preventDefault();
          updateState({
            message: 'Invalid key pressed',
            messageType: 'error',
          });
        }
        break;
    }
  }, [
    state.isSubmitting,
    handleSubmit,
    handleExit,
    handleClear,
    updateState,
  ]);

  useEffect(() => {
    document.addEventListener('keydown', handleKeyDown);
    return () => {
      document.removeEventListener('keydown', handleKeyDown);
    };
  }, [handleKeyDown]);

  useEffect(() => {
    // Focus on first name field when component mounts
    const firstNameInput = document.getElementById('firstName');
    if (firstNameInput) {
      firstNameInput.focus();
    }
  }, []);

  return (
    <div className={`min-h-screen bg-blue-900 p-4 ${className}`}>
      <div className="bg-white rounded-lg shadow-2xl p-6 w-full max-w-2xl mx-auto">
        {/* Header */}
        <div className="text-center mb-6">
          <h1 className="text-2xl font-bold text-blue-900 mb-1">
            CardDemo
          </h1>
          <h2 className="text-lg text-gray-600 mb-2">
            User Add
          </h2>
          <div className="text-sm text-gray-500">
            <p>Program: COUSR01C | Transaction: CU01</p>
            <p>{new Date().toLocaleDateString()} | {new Date().toLocaleTimeString()}</p>
          </div>
        </div>

        {/* Form */}
        <form onSubmit={(e) => { e.preventDefault(); handleSubmit(); }} className="space-y-6">
          {/* First Name Field */}
          <div>
            <label htmlFor="firstName" className="block text-sm font-medium text-gray-700 mb-2">
              First Name:
            </label>
            <input
              id="firstName"
              type="text"
              value={state.formData.firstName}
              onChange={(e) => handleInputChange('firstName', e.target.value)}
              placeholder="Enter First Name"
              maxLength={20}
              className={`w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500 ${
                state.errors.firstName ? 'border-red-500' : ''
              }`}
              disabled={state.isSubmitting}
            />
            {state.errors.firstName && (
              <p className="mt-1 text-sm text-red-600">{state.errors.firstName}</p>
            )}
          </div>

          {/* Last Name Field */}
          <div>
            <label htmlFor="lastName" className="block text-sm font-medium text-gray-700 mb-2">
              Last Name:
            </label>
            <input
              id="lastName"
              type="text"
              value={state.formData.lastName}
              onChange={(e) => handleInputChange('lastName', e.target.value)}
              placeholder="Enter Last Name"
              maxLength={20}
              className={`w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500 ${
                state.errors.lastName ? 'border-red-500' : ''
              }`}
              disabled={state.isSubmitting}
            />
            {state.errors.lastName && (
              <p className="mt-1 text-sm text-red-600">{state.errors.lastName}</p>
            )}
          </div>

          {/* User ID Field */}
          <div>
            <label htmlFor="userId" className="block text-sm font-medium text-gray-700 mb-2">
              User ID:
            </label>
            <input
              id="userId"
              type="text"
              value={state.formData.userId}
              onChange={(e) => handleInputChange('userId', e.target.value)}
              placeholder="Enter User ID"
              maxLength={8}
              className={`w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500 uppercase ${
                state.errors.userId ? 'border-red-500' : ''
              }`}
              disabled={state.isSubmitting}
            />
            {state.errors.userId && (
              <p className="mt-1 text-sm text-red-600">{state.errors.userId}</p>
            )}
          </div>

          {/* Password Field */}
          <div>
            <label htmlFor="password" className="block text-sm font-medium text-gray-700 mb-2">
              Password:
            </label>
            <input
              id="password"
              type="password"
              value={state.formData.password}
              onChange={(e) => handleInputChange('password', e.target.value)}
              placeholder="Enter Password"
              maxLength={8}
              className={`w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500 ${
                state.errors.password ? 'border-red-500' : ''
              }`}
              disabled={state.isSubmitting}
            />
            {state.errors.password && (
              <p className="mt-1 text-sm text-red-600">{state.errors.password}</p>
            )}
          </div>

          {/* User Type Field */}
          <div>
            <label htmlFor="userType" className="block text-sm font-medium text-gray-700 mb-2">
              User Type:
            </label>
            <select
              id="userType"
              value={state.formData.userType}
              onChange={(e) => handleInputChange('userType', e.target.value as UserType)}
              className={`w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500 ${
                state.errors.userType ? 'border-red-500' : ''
              }`}
              disabled={state.isSubmitting}
            >
              <option value="G">{USER_TYPE_LABELS.G}</option>
              <option value="A">{USER_TYPE_LABELS.A}</option>
            </select>
            {state.errors.userType && (
              <p className="mt-1 text-sm text-red-600">{state.errors.userType}</p>
            )}
          </div>

          {/* Message */}
          {state.message && (
            <div className={`p-3 rounded-md ${
              state.messageType === 'success' 
                ? 'bg-green-50 border border-green-200 text-green-800' 
                : 'bg-red-50 border border-red-200 text-red-800'
            }`}>
              <p className="text-sm">{state.message}</p>
            </div>
          )}

          {/* Action Buttons */}
          <div className="flex flex-wrap justify-center space-x-2 pt-6">
            <Button
              type="submit"
              variant="primary"
              disabled={state.isSubmitting}
              size="md"
            >
              {state.isSubmitting ? 'Adding...' : 'Add User (ENTER)'}
            </Button>
            
            <Button
              type="button"
              variant="secondary"
              onClick={handleClear}
              disabled={state.isSubmitting}
              size="md"
            >
              Clear (PF4)
            </Button>
            
            <Button
              type="button"
              variant="secondary"
              onClick={handleExit}
              disabled={state.isSubmitting}
              size="md"
            >
              Exit (PF3)
            </Button>
          </div>
        </form>

        {/* Instructions */}
        <div className="mt-6 text-center text-sm text-gray-500">
          <p>
            Fill in all fields and press ENTER to add user, PF4 to clear, or PF3 to exit
          </p>
        </div>
      </div>
    </div>
  );
};

export default AddUser;