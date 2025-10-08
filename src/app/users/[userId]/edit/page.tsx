'use client';

import React, { useState, useEffect, useCallback } from 'react';
import { useRouter, useParams } from 'next/navigation';
import { Button, Input, Select } from '@/components/ui';
import { userService } from '@/services/userService';
import {
  User,
  UserUpdateRequest,
  UserFormData,
  UserFormErrors,
  USER_TYPE_OPTIONS
} from '@/types/user';

interface UpdateUserPageState {
  lookupUserId: string;
  user: User | null;
  formData: UserFormData;
  originalFormData: UserFormData | null;
  errors: UserFormErrors;
  isLoading: boolean;
  isLookingUp: boolean;
  message: string;
  messageType: 'success' | 'error' | 'info' | '';
  mode: 'lookup' | 'edit';
  hasChanges: boolean;
}

const INITIAL_FORM_DATA: UserFormData = {
  userId: '',
  firstName: '',
  lastName: '',
  password: '',
  userType: ''
};

const INITIAL_STATE: UpdateUserPageState = {
  lookupUserId: '',
  user: null,
  formData: INITIAL_FORM_DATA,
  originalFormData: null,
  errors: {},
  isLoading: false,
  isLookingUp: false,
  message: '',
  messageType: '',
  mode: 'lookup',
  hasChanges: false
};

const MAX_LENGTHS = {
  userId: 8,
  firstName: 20,
  lastName: 20,
  password: 8
};

export default function UpdateUserPage() {
  const router = useRouter();
  const params = useParams();
  const [state, setState] = useState<UpdateUserPageState>(INITIAL_STATE);

  const updateState = useCallback((updates: Partial<UpdateUserPageState>) => {
    setState(prev => ({ ...prev, ...updates }));
  }, []);

  const setMessage = useCallback((message: string, type: 'success' | 'error' | 'info' = 'info') => {
    updateState({ message, messageType: type });
  }, [updateState]);

  const checkForChanges = useCallback((currentData: UserFormData, originalData: UserFormData | null): boolean => {
    if (!originalData) return false;
    
    return (
      currentData.firstName !== originalData.firstName ||
      currentData.lastName !== originalData.lastName ||
      currentData.password !== originalData.password ||
      currentData.userType !== originalData.userType
    );
  }, []);

  const validateField = useCallback((name: keyof UserFormData, value: string): string => {
    switch (name) {
      case 'userId':
        if (!value || value.trim() === '') return 'User ID can NOT be empty...';
        return '';
      
      case 'firstName':
        if (!value || value.trim() === '') return 'First Name can NOT be empty...';
        if (value.length > MAX_LENGTHS.firstName) return `First Name cannot exceed ${MAX_LENGTHS.firstName} characters`;
        return '';
      
      case 'lastName':
        if (!value || value.trim() === '') return 'Last Name can NOT be empty...';
        if (value.length > MAX_LENGTHS.lastName) return `Last Name cannot exceed ${MAX_LENGTHS.lastName} characters`;
        return '';
      
      case 'password':
        if (!value || value.trim() === '') return 'Password can NOT be empty...';
        if (value.length > MAX_LENGTHS.password) return `Password cannot exceed ${MAX_LENGTHS.password} characters`;
        return '';
      
      case 'userType':
        if (!value || value.trim() === '') return 'User Type can NOT be empty...';
        if (!['A', 'R'].includes(value)) return 'User Type must be A (Admin) or R (Regular)';
        return '';
      
      default:
        return '';
    }
  }, []);

  const validateForm = useCallback((): { isValid: boolean; firstErrorField?: keyof UserFormData } => {
    const newErrors: UserFormErrors = {};
    let isValid = true;
    let firstErrorField: keyof UserFormData | undefined;

    // Validate in specific order as per business rules
    const fieldsToValidate: (keyof UserFormData)[] = ['userId', 'firstName', 'lastName', 'password', 'userType'];
    
    for (const fieldName of fieldsToValidate) {
      const value = state.formData[fieldName] as string;
      const error = validateField(fieldName, value);
      
      if (error) {
        newErrors[fieldName] = error;
        if (isValid) {
          firstErrorField = fieldName;
        }
        isValid = false;
      }
    }

    updateState({ errors: newErrors });
    return { isValid, firstErrorField };
  }, [state.formData, validateField, updateState]);

  const handleLookupInputChange = useCallback((value: string) => {
    const processedValue = value.slice(0, MAX_LENGTHS.userId).toUpperCase();
    updateState({ lookupUserId: processedValue });
  }, [updateState]);

  const handleInputChange = useCallback((field: keyof UserFormData, value: string) => {
    let processedValue = value;
    
    switch (field) {
      case 'userId':
        processedValue = value.slice(0, MAX_LENGTHS.userId).toUpperCase();
        break;
      case 'firstName':
        processedValue = value.slice(0, MAX_LENGTHS.firstName);
        break;
      case 'lastName':
        processedValue = value.slice(0, MAX_LENGTHS.lastName);
        break;
      case 'password':
        processedValue = value.slice(0, MAX_LENGTHS.password);
        break;
      case 'userType':
        processedValue = value.slice(0, 1).toUpperCase();
        break;
    }

    const newFormData = {
      ...state.formData,
      [field]: processedValue
    };

    const hasChanges = checkForChanges(newFormData, state.originalFormData);

    const newErrors = { ...state.errors };
    if (newErrors[field]) {
      delete newErrors[field];
    }

    updateState({
      formData: newFormData,
      errors: newErrors,
      hasChanges,
      message: '',
      messageType: ''
    });
  }, [state.formData, state.errors, state.originalFormData, updateState, checkForChanges]);

  const handleEnter = useCallback(async () => {
    if (state.mode === 'lookup') {
      // Lookup user
      if (!state.lookupUserId.trim()) {
        setMessage('User ID can NOT be empty...', 'error');
        return;
      }

      updateState({ isLookingUp: true });

      try {
        const user = await userService.getUserById(state.lookupUserId.trim());
        
        const formData: UserFormData = {
          userId: user.userId,
          firstName: user.firstName,
          lastName: user.lastName,
          password: '', // Don't show existing password
          userType: user.userType
        };

        updateState({
          user,
          formData,
          originalFormData: { ...formData },
          mode: 'edit',
          isLookingUp: false,
          hasChanges: false,
          errors: {}
        });

        setMessage('Press PF5 key to save your updates ...', 'info');

      } catch (error) {
        const errorMessage = error instanceof Error ? error.message : 'Unable to lookup User...';
        
        if (errorMessage.toLowerCase().includes('not found') || errorMessage.toLowerCase().includes('404')) {
          setMessage('User ID NOT found...', 'error');
        } else {
          setMessage('Unable to lookup User...', 'error');
        }
        
        updateState({ isLookingUp: false });
      }
    } else {
      // Update user
      await handleUpdate();
    }
  }, [state.mode, state.lookupUserId, updateState, setMessage]);

  const handleUpdate = useCallback(async () => {
    if (!state.user) {
      setMessage('No user loaded for update', 'error');
      return;
    }

    const validation = validateForm();
    if (!validation.isValid) {
      if (validation.firstErrorField) {
        setMessage(state.errors[validation.firstErrorField] || 'Please correct the errors', 'error');
        // Focus on the first error field
        const element = document.getElementById(validation.firstErrorField);
        if (element) {
          element.focus();
        }
      }
      return;
    }

    if (!state.hasChanges) {
      setMessage('Please modify to update ...', 'error');
      return;
    }

    updateState({ isLoading: true });

    try {
      const updateRequest: UserUpdateRequest = {
        firstName: state.formData.firstName.trim(),
        lastName: state.formData.lastName.trim(),
        password: state.formData.password.trim(),
        userType: state.formData.userType.trim()
      };

      const updatedUser = await userService.updateUser(state.user.userId, updateRequest);
      
      const newFormData: UserFormData = {
        userId: updatedUser.userId,
        firstName: updatedUser.firstName,
        lastName: updatedUser.lastName,
        password: '', // Don't show password
        userType: updatedUser.userType
      };

      updateState({
        user: updatedUser,
        formData: newFormData,
        originalFormData: { ...newFormData },
        hasChanges: false,
        isLoading: false
      });

      setMessage(`User ${updatedUser.userId} has been updated ...`, 'success');

    } catch (error) {
      const errorMessage = error instanceof Error ? error.message : 'Unable to Update User...';
      
      if (errorMessage.toLowerCase().includes('not found') || errorMessage.toLowerCase().includes('404')) {
        setMessage('User ID NOT found...', 'error');
        // Focus on User ID field
        const element = document.getElementById('userId');
        if (element) {
          element.focus();
        }
      } else {
        setMessage('Unable to Update User...', 'error');
        // Focus on First Name field for general errors
        const element = document.getElementById('firstName');
        if (element) {
          element.focus();
        }
      }
      
      updateState({ isLoading: false });
    }
  }, [state.user, state.formData, state.hasChanges, state.errors, validateForm, updateState, setMessage]);

  const handlePF3SaveAndExit = useCallback(async () => {
    if (state.mode === 'edit' && state.hasChanges) {
      await handleUpdate();
      setTimeout(() => {
        router.push('/admin');
      }, 1000);
    } else {
      router.push('/admin');
    }
  }, [state.mode, state.hasChanges, handleUpdate, router]);

  const handlePF4Clear = useCallback(() => {
    if (state.mode === 'lookup') {
      updateState({
        lookupUserId: '',
        message: '',
        messageType: ''
      });
    } else {
      if (state.originalFormData) {
        updateState({
          formData: { ...state.originalFormData },
          errors: {},
          hasChanges: false,
          message: '',
          messageType: ''
        });
      }
    }
  }, [state.mode, state.originalFormData, updateState]);

  const handlePF5Update = useCallback(async () => {
    if (state.mode === 'edit') {
      await handleUpdate();
    }
  }, [state.mode, handleUpdate]);

  const handlePF12Cancel = useCallback(() => {
    router.push('/admin');
  }, [router]);

  const handleKeyDown = useCallback((event: KeyboardEvent) => {
    switch (event.key) {
      case 'Enter':
        event.preventDefault();
        if (!state.isLoading && !state.isLookingUp) {
          handleEnter();
        }
        break;
      
      case 'F3':
        event.preventDefault();
        handlePF3SaveAndExit();
        break;
      
      case 'F4':
        event.preventDefault();
        handlePF4Clear();
        break;
      
      case 'F5':
        event.preventDefault();
        if (!state.isLoading) {
          handlePF5Update();
        }
        break;
      
      case 'F12':
        event.preventDefault();
        handlePF12Cancel();
        break;
      
      default:
        if (event.key.startsWith('F') && !['F3', 'F4', 'F5', 'F12'].includes(event.key)) {
          event.preventDefault();
          setMessage('INVALID KEY PRESSED', 'error');
        }
        break;
    }
  }, [state.isLoading, state.isLookingUp, handleEnter, handlePF3SaveAndExit, handlePF4Clear, handlePF5Update, handlePF12Cancel, setMessage]);

  // Set up keyboard event listeners
  useEffect(() => {
    document.addEventListener('keydown', handleKeyDown);
    return () => {
      document.removeEventListener('keydown', handleKeyDown);
    };
  }, [handleKeyDown]);

  // Check if userId is provided in URL params
  useEffect(() => {
    if (params.userId && typeof params.userId === 'string') {
      updateState({ lookupUserId: params.userId });
      // Auto-lookup if userId is provided
      setTimeout(() => {
        handleEnter();
      }, 100);
    }
  }, [params.userId, updateState]);

  // Focus on lookup field on mount
  useEffect(() => {
    if (state.mode === 'lookup') {
      const element = document.getElementById('lookupUserId');
      if (element) {
        element.focus();
      }
    }
  }, [state.mode]);

  return (
    <div className="min-h-screen bg-gray-50 p-6">
      <div className="max-w-2xl mx-auto">
        {/* Header */}
        <div className="bg-white rounded-lg shadow-sm border border-gray-200 p-6 mb-6">
          <div className="flex justify-between items-center">
            <div>
              <h1 className="text-2xl font-bold text-gray-900 mb-2">CARDDEMO</h1>
              <h2 className="text-lg text-gray-700">User Update</h2>
            </div>
            <div className="text-right text-sm text-gray-600">
              <div>Program: COUSR02C</div>
              <div>Transaction: CU02</div>
              <div>{new Date().toLocaleDateString()}</div>
              <div>{new Date().toLocaleTimeString()}</div>
            </div>
          </div>
        </div>

        {/* Message Display */}
        {state.message && (
          <div className={`mb-6 p-4 rounded-lg border ${
            state.messageType === 'success' ? 'bg-green-50 border-green-200 text-green-800' :
            state.messageType === 'error' ? 'bg-red-50 border-red-200 text-red-800' :
            'bg-blue-50 border-blue-200 text-blue-800'
          }`}>
            {state.message}
          </div>
        )}

        {/* Lookup Section */}
        {state.mode === 'lookup' && (
          <div className="bg-white rounded-lg shadow-sm border border-gray-200 p-6 mb-6">
            <div className="space-y-4">
              <div>
                <label htmlFor="lookupUserId" className="block text-sm font-medium text-gray-700 mb-2">
                  User ID <span className="text-red-500">*</span>
                </label>
                <Input
                  id="lookupUserId"
                  type="text"
                  value={state.lookupUserId}
                  onChange={(e) => handleLookupInputChange(e.target.value)}
                  placeholder="Enter User ID to lookup"
                  className="w-full"
                  disabled={state.isLookingUp}
                  maxLength={MAX_LENGTHS.userId}
                />
              </div>
            </div>
          </div>
        )}

        {/* Edit Form */}
        {state.mode === 'edit' && state.user && (
          <div className="bg-white rounded-lg shadow-sm border border-gray-200 p-6 mb-6">
            <div className="flex justify-between items-center mb-4">
              <h3 className="text-lg font-semibold text-gray-900">Edit User Information</h3>
              {state.hasChanges && (
                <span className="text-sm text-orange-600 bg-orange-50 px-2 py-1 rounded">
                  Modified
                </span>
              )}
            </div>
            
            <form onSubmit={(e) => { e.preventDefault(); handleUpdate(); }} className="space-y-6">
              {/* User ID Field (Read-only) */}
              <div>
                <label htmlFor="userId" className="block text-sm font-medium text-gray-700 mb-2">
                  User ID
                </label>
                <Input
                  id="userId"
                  type="text"
                  value={state.formData.userId}
                  className="w-full bg-gray-50"
                  disabled={true}
                  readOnly={true}
                />
              </div>

              {/* First Name Field */}
              <div>
                <label htmlFor="firstName" className="block text-sm font-medium text-gray-700 mb-2">
                  First Name <span className="text-red-500">*</span>
                  <span className="text-gray-500 text-xs ml-2">(Max {MAX_LENGTHS.firstName} characters)</span>
                </label>
                <Input
                  id="firstName"
                  type="text"
                  value={state.formData.firstName}
                  onChange={(e) => handleInputChange('firstName', e.target.value)}
                  placeholder="Enter First Name"
                  className={`w-full ${state.errors.firstName ? 'border-red-500 focus:border-red-500' : ''}`}
                  disabled={state.isLoading}
                  maxLength={MAX_LENGTHS.firstName}
                />
                {state.errors.firstName && (
                  <p className="mt-1 text-sm text-red-600">{state.errors.firstName}</p>
                )}
              </div>

              {/* Last Name Field */}
              <div>
                <label htmlFor="lastName" className="block text-sm font-medium text-gray-700 mb-2">
                  Last Name <span className="text-red-500">*</span>
                  <span className="text-gray-500 text-xs ml-2">(Max {MAX_LENGTHS.lastName} characters)</span>
                </label>
                <Input
                  id="lastName"
                  type="text"
                  value={state.formData.lastName}
                  onChange={(e) => handleInputChange('lastName', e.target.value)}
                  placeholder="Enter Last Name"
                  className={`w-full ${state.errors.lastName ? 'border-red-500 focus:border-red-500' : ''}`}
                  disabled={state.isLoading}
                  maxLength={MAX_LENGTHS.lastName}
                />
                {state.errors.lastName && (
                  <p className="mt-1 text-sm text-red-600">{state.errors.lastName}</p>
                )}
              </div>

              {/* Password Field */}
              <div>
                <label htmlFor="password" className="block text-sm font-medium text-gray-700 mb-2">
                  Password <span className="text-red-500">*</span>
                  <span className="text-gray-500 text-xs ml-2">(Max {MAX_LENGTHS.password} characters)</span>
                </label>
                <Input
                  id="password"
                  type="password"
                  value={state.formData.password}
                  onChange={(e) => handleInputChange('password', e.target.value)}
                  placeholder="Enter Password"
                  className={`w-full ${state.errors.password ? 'border-red-500 focus:border-red-500' : ''}`}
                  disabled={state.isLoading}
                  maxLength={MAX_LENGTHS.password}
                />
                {state.errors.password && (
                  <p className="mt-1 text-sm text-red-600">{state.errors.password}</p>
                )}
              </div>

              {/* User Type Field */}
              <div>
                <label htmlFor="userType" className="block text-sm font-medium text-gray-700 mb-2">
                  User Type <span className="text-red-500">*</span>
                </label>
                <Select
                  id="userType"
                  value={state.formData.userType}
                  onChange={(e) => handleInputChange('userType', e.target.value)}
                  options={[
                    { value: '', label: 'Select User Type' },
                    ...USER_TYPE_OPTIONS
                  ]}
                  className={`w-full ${state.errors.userType ? 'border-red-500 focus:border-red-500' : ''}`}
                  disabled={state.isLoading}
                />
                {state.errors.userType && (
                  <p className="mt-1 text-sm text-red-600">{state.errors.userType}</p>
                )}
              </div>
            </form>
          </div>
        )}

        {/* Action Buttons */}
        <div className="bg-white rounded-lg shadow-sm border border-gray-200 p-6 mb-6">
          <div className="flex flex-wrap gap-4 justify-between">
            <div className="flex gap-3">
              <Button
                onClick={handleEnter}
                disabled={state.isLoading || state.isLookingUp}
                className="bg-blue-600 hover:bg-blue-700 text-white"
              >
                {state.isLookingUp ? 'Looking up...' : 
                 state.isLoading ? 'Updating...' : 
                 state.mode === 'lookup' ? 'Enter - Lookup' : 'Enter - Update'}
              </Button>
              
              {state.mode === 'edit' && (
                <Button
                  onClick={handlePF5Update}
                  disabled={state.isLoading || !state.hasChanges}
                  className="bg-green-600 hover:bg-green-700 text-white"
                >
                  F5 - Update
                </Button>
              )}
              
              <Button
                onClick={handlePF4Clear}
                disabled={state.isLoading || state.isLookingUp}
                variant="outline"
              >
                F4 - Clear
              </Button>
            </div>

            <div className="flex gap-2">
              <Button
                onClick={handlePF3SaveAndExit}
                disabled={state.isLoading || state.isLookingUp}
                variant="outline"
              >
                F3 - {state.mode === 'edit' && state.hasChanges ? 'Save & Exit' : 'Exit'}
              </Button>
              <Button
                onClick={handlePF12Cancel}
                disabled={state.isLoading || state.isLookingUp}
                variant="outline"
                className="text-gray-600 hover:text-gray-800"
              >
                F12 - Cancel
              </Button>
            </div>
          </div>
        </div>

        {/* Instructions */}
        <div className="bg-blue-50 border border-blue-200 rounded-lg p-4">
          <h4 className="font-semibold text-blue-900 mb-2">Instructions:</h4>
          <ul className="text-sm text-blue-800 space-y-1">
            {state.mode === 'lookup' ? (
              <>
                <li>• Enter a User ID and press Enter to lookup</li>
                <li>• Press F3 to exit to Admin Menu</li>
                <li>• Press F4 to clear the lookup field</li>
              </>
            ) : (
              <>
                <li>• Modify the user information as needed</li>
                <li>• Press Enter or F5 to update the user</li>
                <li>• Press F3 to save changes and exit</li>
                <li>• Press F4 to reset to original values</li>
                <li>• Press F12 to cancel without saving</li>
              </>
            )}
          </ul>
        </div>
      </div>
    </div>
  );
}