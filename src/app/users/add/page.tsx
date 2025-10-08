'use client';

import { useState, useEffect, useCallback } from 'react';
import { useRouter } from 'next/navigation';
import { Button, Input, Select } from '@/components/ui';
import userSecurityService from '@/services/userSecurityService';
import {
  CreateUserSecurityRequest,
  UserRole,
  FormErrors,
  ExitRequest,
  InvalidKeyRequest,
  COUSR01CRequest,
  COUSRResponse
} from '@/types/userSecurity';

interface AddUserState {
  userId: string;
  firstName: string;
  lastName: string;
  password: string;
  userType: string;
  isLoading: boolean;
  error: string;
  message: string;
  isSubmitting: boolean;
}

export default function AddUserPage() {
  const router = useRouter();
  const [state, setState] = useState<AddUserState>({
    userId: '',
    firstName: '',
    lastName: '',
    password: '',
    userType: '',
    isLoading: false,
    error: '',
    message: '',
    isSubmitting: false
  });
  const [errors, setErrors] = useState<FormErrors>({});
  const [isInitialized, setIsInitialized] = useState(false);

  const userTypeOptions = [
    { value: '', label: 'Select User Type' },
    { value: UserRole.ADMIN, label: 'Administrator' },
    { value: UserRole.USER, label: 'User' },
    { value: UserRole.MANAGER, label: 'Manager' },
    { value: UserRole.VIEWER, label: 'Viewer' }
  ];

  // Initialize page
  useEffect(() => {
    const initializePage = async () => {
      setIsInitialized(true);
      setState(prev => ({ 
        ...prev, 
        message: 'Enter user details and press ENTER to save, PF3 to save and exit, PF4 to clear, or PF12 to cancel' 
      }));
    };
    
    initializePage();
  }, []);

  // Validate form fields
  const validateForm = useCallback((): boolean => {
    const newErrors: FormErrors = {};

    // User ID validation
    if (!state.userId.trim()) {
      newErrors.userId = 'User ID is required';
    } else if (state.userId.trim().length < 3) {
      newErrors.userId = 'User ID must be at least 3 characters';
    } else if (state.userId.trim().length > 20) {
      newErrors.userId = 'User ID must not exceed 20 characters';
    } else if (!/^[a-zA-Z0-9_]+$/.test(state.userId.trim())) {
      newErrors.userId = 'User ID can only contain letters, numbers, and underscores';
    }

    // First Name validation
    if (!state.firstName.trim()) {
      newErrors.firstName = 'First Name is required';
    } else if (state.firstName.trim().length < 2) {
      newErrors.firstName = 'First Name must be at least 2 characters';
    } else if (state.firstName.trim().length > 50) {
      newErrors.firstName = 'First Name must not exceed 50 characters';
    }

    // Last Name validation
    if (!state.lastName.trim()) {
      newErrors.lastName = 'Last Name is required';
    } else if (state.lastName.trim().length < 2) {
      newErrors.lastName = 'Last Name must be at least 2 characters';
    } else if (state.lastName.trim().length > 50) {
      newErrors.lastName = 'Last Name must not exceed 50 characters';
    }

    // Password validation
    if (!state.password.trim()) {
      newErrors.password = 'Password is required';
    } else if (state.password.length < 8) {
      newErrors.password = 'Password must be at least 8 characters';
    } else if (state.password.length > 128) {
      newErrors.password = 'Password must not exceed 128 characters';
    } else if (!/(?=.*[a-z])(?=.*[A-Z])(?=.*\d)/.test(state.password)) {
      newErrors.password = 'Password must contain at least one uppercase letter, one lowercase letter, and one number';
    }

    // User Type validation
    if (!state.userType.trim()) {
      newErrors.userType = 'User Type is required';
    } else if (!Object.values(UserRole).includes(state.userType as UserRole)) {
      newErrors.userType = 'Invalid User Type selected';
    }

    setErrors(newErrors);
    return Object.keys(newErrors).length === 0;
  }, [state.userId, state.firstName, state.lastName, state.password, state.userType]);

  // Check for duplicate User ID
  const checkDuplicateUserId = useCallback(async (userId: string): Promise<boolean> => {
    try {
      const response = await userSecurityService.getUserById(userId);
      return response.success && !!response.data;
    } catch (error) {
      console.error('Error checking duplicate user ID:', error);
      return false;
    }
  }, []);

  // Execute COUSR01C business logic
  const executeCOUSR01C = useCallback(async (userData: CreateUserSecurityRequest): Promise<COUSRResponse> => {
    try {
      const cousr01cRequest: COUSR01CRequest = {
        action: 'CREATE',
        userData
      };

      // This would typically call the business logic service
      // For now, we'll simulate the business logic validation
      const response: COUSRResponse = {
        returnCode: '0000',
        message: 'User creation validation successful',
        data: userData
      };

      return response;
    } catch (error) {
      console.error('COUSR01C execution error:', error);
      return {
        returnCode: '9999',
        message: 'Business logic validation failed',
        errors: [{ code: 'SYSTEM_ERROR', message: 'System error occurred', field: 'system' }]
      };
    }
  }, []);

  // Handle form submission
  const handleSubmit = useCallback(async (saveAndExit: boolean = false): Promise<void> => {
    if (state.isSubmitting) {
      return;
    }

    setState(prev => ({ ...prev, error: '', message: '', isSubmitting: true }));

    try {
      // Validate form
      if (!validateForm()) {
        setState(prev => ({ 
          ...prev, 
          error: 'Please correct the validation errors before submitting',
          isSubmitting: false 
        }));
        return;
      }

      // Check for duplicate User ID
      const isDuplicate = await checkDuplicateUserId(state.userId.trim());
      if (isDuplicate) {
        setErrors({ userId: 'User ID already exists. Please choose a different User ID.' });
        setState(prev => ({ 
          ...prev, 
          error: 'User ID already exists',
          isSubmitting: false 
        }));
        return;
      }

      // Prepare user data
      const userData: CreateUserSecurityRequest = {
        username: state.userId.trim(),
        email: `${state.userId.trim().toLowerCase()}@company.com`, // Generate email from userId
        firstName: state.firstName.trim(),
        lastName: state.lastName.trim(),
        password: state.password,
        confirmPassword: state.password,
        roles: [state.userType as UserRole],
        permissions: [],
        isActive: true,
        mustChangePassword: true
      };

      // Execute COUSR01C business logic
      const businessLogicResponse = await executeCOUSR01C(userData);
      
      if (businessLogicResponse.returnCode !== '0000') {
        setState(prev => ({ 
          ...prev, 
          error: businessLogicResponse.message || 'Business logic validation failed',
          isSubmitting: false 
        }));
        return;
      }

      // Create user via service
      const response = await userSecurityService.createUser(userData);

      if (response.success) {
        const successMessage = `User ${state.userId} created successfully`;
        setState(prev => ({ 
          ...prev, 
          message: successMessage,
          isSubmitting: false 
        }));

        if (saveAndExit) {
          // Wait a moment to show success message, then navigate
          setTimeout(() => {
            router.push('/users');
          }, 1500);
        } else {
          // Clear form for next entry
          setTimeout(() => {
            handleClearForm();
          }, 2000);
        }
      } else {
        const errorMessage = response.error || 'Failed to create user';
        
        // Handle specific error cases
        if (errorMessage.toLowerCase().includes('duplicate') || errorMessage.toLowerCase().includes('exists')) {
          setErrors({ userId: 'User ID already exists. Please choose a different User ID.' });
        }
        
        setState(prev => ({ 
          ...prev, 
          error: errorMessage,
          isSubmitting: false 
        }));
      }
    } catch (error) {
      console.error('Submit error:', error);
      setState(prev => ({ 
        ...prev, 
        error: 'An unexpected error occurred while creating the user',
        isSubmitting: false 
      }));
    }
  }, [state, validateForm, checkDuplicateUserId, executeCOUSR01C, router]);

  // Handle clear form (PF4)
  const handleClearForm = useCallback((): void => {
    setState({
      userId: '',
      firstName: '',
      lastName: '',
      password: '',
      userType: '',
      isLoading: false,
      error: '',
      message: 'Form cleared. Enter user details.',
      isSubmitting: false
    });
    setErrors({});
  }, []);

  // Handle cancel (PF12)
  const handleCancel = useCallback(async (): Promise<void> => {
    setState(prev => ({ ...prev, isLoading: true }));

    try {
      const exitRequest: ExitRequest = {};
      await userSecurityService.exit(exitRequest);
      
      router.push('/users');
    } catch (error) {
      console.error('Cancel error:', error);
      router.push('/users');
    } finally {
      setState(prev => ({ ...prev, isLoading: false }));
    }
  }, [router]);

  // Handle invalid key press
  const handleInvalidKey = useCallback(async (key: string): Promise<void> => {
    try {
      const invalidKeyRequest: InvalidKeyRequest = {
        attemptedKey: key,
        timestamp: new Date().toISOString()
      };

      const response = await userSecurityService.handleInvalidKey(invalidKeyRequest);
      
      if (response.success && response.data?.message) {
        setState(prev => ({ ...prev, error: response.data!.message }));
      } else {
        setState(prev => ({ 
          ...prev, 
          error: `Invalid key pressed: ${key}. Use ENTER to save, PF3 to save and exit, PF4 to clear, or PF12 to cancel.` 
        }));
      }
    } catch (error) {
      console.error('Invalid key handling error:', error);
      setState(prev => ({ 
        ...prev, 
        error: `Invalid key pressed: ${key}. Use ENTER to save, PF3 to save and exit, PF4 to clear, or PF12 to cancel.` 
      }));
    }
  }, []);

  // Handle keyboard events
  const handleKeyDown = useCallback(async (event: React.KeyboardEvent): Promise<void> => {
    // Prevent default for function keys
    if (event.key.startsWith('F') || event.key === 'Enter') {
      event.preventDefault();
    }

    if (state.isSubmitting || state.isLoading) {
      return;
    }

    // Clear previous messages
    setState(prev => ({ ...prev, error: '', message: '' }));

    switch (event.key) {
      case 'Enter':
        await handleSubmit(false);
        break;
      
      case 'F3':
        await handleSubmit(true);
        break;
      
      case 'F4':
        handleClearForm();
        break;
      
      case 'F12':
        await handleCancel();
        break;
      
      case 'F1':
      case 'F2':
      case 'F5':
      case 'F6':
      case 'F7':
      case 'F8':
      case 'F9':
      case 'F10':
      case 'F11':
        await handleInvalidKey(event.key);
        break;
      
      default:
        // Allow normal typing
        if (!event.ctrlKey && !event.altKey && !event.metaKey) {
          return;
        }
        
        // Handle other special key combinations as invalid
        if (event.ctrlKey || event.altKey || event.metaKey) {
          await handleInvalidKey(`${event.ctrlKey ? 'Ctrl+' : ''}${event.altKey ? 'Alt+' : ''}${event.metaKey ? 'Meta+' : ''}${event.key}`);
        }
        break;
    }
  }, [state.isSubmitting, state.isLoading, handleSubmit, handleClearForm, handleCancel, handleInvalidKey]);

  // Handle input changes
  const handleInputChange = useCallback((field: keyof AddUserState, value: string): void => {
    setState(prev => ({ ...prev, [field]: value }));
    
    // Clear field-specific error when user starts typing
    if (errors[field]) {
      setErrors(prev => ({ ...prev, [field]: undefined }));
    }
  }, [errors]);

  // Don't render until initialized
  if (!isInitialized) {
    return (
      <div className="min-h-screen flex items-center justify-center bg-gray-50">
        <div className="text-center">
          <div className="animate-spin rounded-full h-8 w-8 border-b-2 border-blue-600 mx-auto"></div>
          <p className="mt-2 text-gray-600">Loading...</p>
        </div>
      </div>
    );
  }

  return (
    <div 
      className="min-h-screen bg-gray-50 py-8 px-4 sm:px-6 lg:px-8"
      onKeyDown={handleKeyDown}
      tabIndex={-1}
    >
      <div className="max-w-2xl mx-auto">
        {/* Header */}
        <div className="mb-8">
          <h1 className="text-3xl font-bold text-gray-900">Add User (COUSR01C)</h1>
          <p className="mt-2 text-gray-600">
            Create a new user account with required information
          </p>
        </div>

        {/* Messages */}
        {state.error && (
          <div className="mb-6 rounded-md bg-red-50 p-4">
            <div className="flex">
              <div className="flex-shrink-0">
                <svg className="h-5 w-5 text-red-400" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 20 20" fill="currentColor">
                  <path fillRule="evenodd" d="M10 18a8 8 0 100-16 8 8 0 000 16zM8.707 7.293a1 1 0 00-1.414 1.414L8.586 10l-1.293 1.293a1 1 0 101.414 1.414L10 11.414l1.293 1.293a1 1 0 001.414-1.414L11.414 10l1.293-1.293a1 1 0 00-1.414-1.414L10 8.586 8.707 7.293z" clipRule="evenodd" />
                </svg>
              </div>
              <div className="ml-3">
                <p className="text-sm text-red-800">{state.error}</p>
              </div>
            </div>
          </div>
        )}

        {state.message && !state.error && (
          <div className="mb-6 rounded-md bg-green-50 p-4">
            <div className="flex">
              <div className="flex-shrink-0">
                <svg className="h-5 w-5 text-green-400" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 20 20" fill="currentColor">
                  <path fillRule="evenodd" d="M10 18a8 8 0 100-16 8 8 0 000 16zm3.707-9.293a1 1 0 00-1.414-1.414L9 10.586 7.707 9.293a1 1 0 00-1.414 1.414l2 2a1 1 0 001.414 0l4-4z" clipRule="evenodd" />
                </svg>
              </div>
              <div className="ml-3">
                <p className="text-sm text-green-800">{state.message}</p>
              </div>
            </div>
          </div>
        )}

        {/* Form */}
        <div className="bg-white shadow rounded-lg p-6">
          <form className="space-y-6" onSubmit={(e) => e.preventDefault()}>
            {/* User ID */}
            <div>
              <label htmlFor="userId" className="block text-sm font-medium text-gray-700">
                User ID <span className="text-red-500">*</span>
              </label>
              <Input
                id="userId"
                name="userId"
                type="text"
                value={state.userId}
                onChange={(e) => handleInputChange('userId', e.target.value)}
                placeholder="Enter unique User ID"
                disabled={state.isSubmitting}
                className={`mt-1 ${errors.userId ? 'border-red-500 focus:border-red-500 focus:ring-red-500' : ''}`}
                maxLength={20}
              />
              {errors.userId && (
                <p className="mt-1 text-sm text-red-600">{errors.userId}</p>
              )}
            </div>

            {/* First Name */}
            <div>
              <label htmlFor="firstName" className="block text-sm font-medium text-gray-700">
                First Name <span className="text-red-500">*</span>
              </label>
              <Input
                id="firstName"
                name="firstName"
                type="text"
                value={state.firstName}
                onChange={(e) => handleInputChange('firstName', e.target.value)}
                placeholder="Enter first name"
                disabled={state.isSubmitting}
                className={`mt-1 ${errors.firstName ? 'border-red-500 focus:border-red-500 focus:ring-red-500' : ''}`}
                maxLength={50}
              />
              {errors.firstName && (
                <p className="mt-1 text-sm text-red-600">{errors.firstName}</p>
              )}
            </div>

            {/* Last Name */}
            <div>
              <label htmlFor="lastName" className="block text-sm font-medium text-gray-700">
                Last Name <span className="text-red-500">*</span>
              </label>
              <Input
                id="lastName"
                name="lastName"
                type="text"
                value={state.lastName}
                onChange={(e) => handleInputChange('lastName', e.target.value)}
                placeholder="Enter last name"
                disabled={state.isSubmitting}
                className={`mt-1 ${errors.lastName ? 'border-red-500 focus:border-red-500 focus:ring-red-500' : ''}`}
                maxLength={50}
              />
              {errors.lastName && (
                <p className="mt-1 text-sm text-red-600">{errors.lastName}</p>
              )}
            </div>

            {/* Password */}
            <div>
              <label htmlFor="password" className="block text-sm font-medium text-gray-700">
                Password <span className="text-red-500">*</span>
              </label>
              <Input
                id="password"
                name="password"
                type="password"
                value={state.password}
                onChange={(e) => handleInputChange('password', e.target.value)}
                placeholder="Enter secure password"
                disabled={state.isSubmitting}
                className={`mt-1 ${errors.password ? 'border-red-500 focus:border-red-500 focus:ring-red-500' : ''}`}
                maxLength={128}
              />
              {errors.password && (
                <p className="mt-1 text-sm text-red-600">{errors.password}</p>
              )}
              <p className="mt-1 text-xs text-gray-500">
                Password must be at least 8 characters with uppercase, lowercase, and number
              </p>
            </div>

            {/* User Type */}
            <div>
              <label htmlFor="userType" className="block text-sm font-medium text-gray-700">
                User Type <span className="text-red-500">*</span>
              </label>
              <Select
                id="userType"
                name="userType"
                value={state.userType}
                onChange={(e) => handleInputChange('userType', e.target.value)}
                disabled={state.isSubmitting}
                className={`mt-1 ${errors.userType ? 'border-red-500 focus:border-red-500 focus:ring-red-500' : ''}`}
                options={userTypeOptions}
              />
              {errors.userType && (
                <p className="mt-1 text-sm text-red-600">{errors.userType}</p>
              )}
            </div>
          </form>
        </div>

        {/* Action Buttons */}
        <div className="mt-6 flex justify-between">
          <div className="flex space-x-2">
            <Button
              onClick={() => handleSubmit(false)}
              disabled={state.isSubmitting}
              className="px-6 py-2"
            >
              {state.isSubmitting ? 'Creating...' : 'Save (ENTER)'}
            </Button>
            <Button
              onClick={() => handleSubmit(true)}
              disabled={state.isSubmitting}
              variant="secondary"
              className="px-6 py-2"
            >
              Save & Exit (PF3)
            </Button>
          </div>

          <div className="flex space-x-2">
            <Button
              onClick={handleClearForm}
              disabled={state.isSubmitting}
              variant="secondary"
              className="px-6 py-2"
            >
              Clear (PF4)
            </Button>
            <Button
              onClick={handleCancel}
              disabled={state.isSubmitting}
              variant="secondary"
              className="px-6 py-2"
            >
              Cancel (PF12)
            </Button>
          </div>
        </div>

        {/* Instructions */}
        <div className="mt-6 bg-gray-50 rounded-lg p-4">
          <h3 className="text-sm font-medium text-gray-900 mb-2">Instructions:</h3>
          <ul className="text-sm text-gray-600 space-y-1">
            <li>• Fill in all required fields marked with <span className="text-red-500">*</span></li>
            <li>• Press ENTER to save and continue adding users</li>
            <li>• Press PF3 to save and exit to users list</li>
            <li>• Press PF4 to clear the form</li>
            <li>• Press PF12 to cancel and return to users list</li>
          </ul>
        </div>
      </div>
    </div>
  );
}